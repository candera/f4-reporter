(ns org.craigandera.parst
  "A parser library. Developed around the concept of `consumers`, a
  function that accepts a sequence of unconsumed strings, and returns
  a map with keys `:parsed` and `:unconsumed`, representing the parsed
  values and the part of the input not consumed."
  (:require [clojure.string :as str]))

(def log (constantly nil))

(defn tokenize
  "Break an input string up by tokens. Tokens can be either strings,
  which will match literally, or keywords, which will match up to the
  next string token. The return value is a map with keys the same as
  the keyword tokens, whose values are the regions of the string
  matched to that token. Returns nil if the string could not be
  matched."
  [^String input tokens]
  (log "tokenize" :input (pr-str input) :tokens (pr-str tokens))
  (when input
    (loop [pos                   (long 0)
           pending-token         nil
           pending-converter     nil
           token-starts          nil
           data                  {}
           [token & more-tokens] tokens]
      (log :pos (pr-str pos)
           :pending-token (pr-str pending-token)
           :pending-converter (pr-str pending-converter)
           :token-start (pr-str token-starts)
           :data (pr-str data)
           :token (pr-str token)
           :more-tokens (pr-str more-tokens))
      (cond
        (nil? token)
        (if pending-token
          (assoc data
                 pending-token
                 (pending-converter (subs input token-starts)))
          data)

        (or (string? token)
            (and (vector? token)
                 (keyword? (first token))
                 (string? (second token))))
        (let [[k ^String t] (if (vector? token)
                      token
                      [nil token])]
          (if pending-token
            (let [index (.indexOf input t pos)]
              (if (pos? index)
                (recur (+ index (count t))
                       nil
                       nil
                       nil
                       (cond-> (assoc data
                                      pending-token
                                      (pending-converter (subs input token-starts index)))
                         k (assoc k t))
                       more-tokens)))
            (when (.startsWith (subs input pos) t)
              (recur (+ pos (count t))
                     nil
                     nil
                     nil
                     (if k
                       (assoc data k t)
                       data)
                     more-tokens))))

        (or (keyword? token)
            (and (vector? token)
                 (keyword? (first token))
                 (ifn? (second token))))
        (let [[k converter] (if (vector? token)
                              token
                              [token identity])]
          (recur pos
                 k
                 converter
                 pos
                 data
                 more-tokens))))))

;; Abstraction over a parse result
(defn ->result
  "Build a result."
  [unconsumed parsed]
  {:unconsumed unconsumed
   :parsed     parsed})

(def unconsumed :unconsumed)
(def parsed :parsed)

(defn transform
  "Apply a transformation to the parsed value of a result."
  [f result]
  (update-in result [:parsed] f))

(defn succeeded?
  "Returns true if result represents a successful parse."
  [result]
  (-> result :parsed some?))

(defn tokenizer
  "Returns a consumer that will consume as per `tokenize`."
  [tokens]
  (fn [[string & more-strings :as strings]]
    (if-let [data (tokenize string tokens)]
      {:unconsumed more-strings
       :parsed data}
      {:unconsumed strings})))

(defn consume-all
  "A consumer that consumes by sequentially applying `consumers`.
  Succeeds only if all consumers succeed. Combines individual results
  with the combiner. The initial value of combiner is the result of
  calling it with zero arguments."
  [strings combiner consumers]
  (loop [ss strings
         data (combiner)
         [consumer & more] consumers]
    (if consumer
      (let [result (consumer ss)]
        (if-not (succeeded? result)
          (->result strings nil)
          (recur (unconsumed result) (combiner data (parsed result)) more)))
      (->result ss data))))

(defn consume-map
  "Consume the data in `strings` according to `pairs`, a set of
  key/consumer pairs. Return result will be a map from each key to the
  result of each consumer. All consumers must succeed for the overall
  consumption to succeed."
  [strings & pairs]
  (loop [[[k consumer] & more] (partition 2 pairs)
         ss strings
         m {}]
    (if-not k
      (->result ss m)
      (let [result (consumer ss)]
        (if-not (succeeded? result)
          (->result strings nil)
          (recur more (unconsumed result) (assoc m k (parsed result))))))))

(defn consume*
  "Returns a consumer that consumes one or more of whatever consumer
  consumes. Returns a result whose parsed value is a sequence of each
  parsed value."
  [consumer]
  (fn [strings]
    (loop [strings strings
           acc []]
      (let [result (consumer strings)]
        (if-not (succeeded? result)
          (->result (unconsumed result) acc)
          (recur (unconsumed result) (conj (if (nil? acc) [] acc)
                                           (parsed result))))))))

(defn consume+
  "Returns a consumer that consumes zero or more of whatever consumer
  consumes. Returns a result whose parsed value is a sequence of each
  parsed value, or nil if there was not at least one.."
  [consumer]
  (let [c (consume* consumer)]
    (fn [strings]
      (let [result (c strings)]
        (if (empty? (parsed result))
          (->result strings nil)
          result)))))

(defn ->map
  "Returns a consumer that consumes whatever consumer does, but wraps
  the result in a one-element map with key k."
  [k consumer]
  (fn [strings]
    (update-in (consumer strings)
               [:parsed]
               (fn [v]
                 (when v
                   {k v})))))

(defn alternative-consumer
  "Returns a consumer that will return the results of the first
  consumer that returns non-nil data."
  [alternatives]
  (fn [strings]
    (loop [[alternative & more] alternatives]
      (if-not alternative
        (->result strings nil)
        (let [result (alternative strings)]
          (if (succeeded? result)
            result
            (recur more)))))))

