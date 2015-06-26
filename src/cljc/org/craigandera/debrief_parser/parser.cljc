(ns org.craigandera.debrief-parser.parser
  "A parser for Falcon debrief files"
  (:require [clojure.string :as str]))

(defn tokenize
  "Break an input string up by tokens. Tokens can be either strings,
  which will match literally, or keywords, which will match up to the
  next string token. The return value is a map with keys the same as
  the keyword tokens, whose values are the regions of the string
  matched to that token. Returns nil if the string could not be
  matched."
  [^String input tokens]
  (loop [pos                   0
         pending-token         nil
         pending-converter     nil
         token-starts          nil
         data                  {}
         [token & more-tokens] tokens]
    #_(println :pos pos
               :pending-token pending-token
               :pending-converter pending-converter
               :token-start token-starts
               :data data
               :token token
               :more-tokens more-tokens)
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
      (let [[k t] (if (vector? token)
                    token
                    [nil token])]
        (if pending-token
          (let [index (.indexOf input t pos)]
            (if (pos? index)
              (recur (+ index (.length t))
                     nil
                     nil
                     nil
                     (cond-> (assoc data
                                    pending-token
                                    (pending-converter (subs input token-starts index)))
                       k (assoc k t))
                     more-tokens)))
          (when (.startsWith (subs input pos) t)
            (recur (+ pos (.length t))
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
               more-tokens)))))

(defn tokenized-line
  [tokens]
  (fn [[line & more-lines :as lines]]
    (if-let [data (tokenize line tokens)]
      [more-lines data]
      [lines nil])))


(defn separator?
  [line]
  (every? #(= \- %) line))

(defn consume
  [lines combiner ops]
  (loop [lines lines
         data (combiner)
         [op & more] ops]
    (if op
      (let [[lines d] (op lines)]
        (recur lines (combiner data d) more))
      [lines data])))

(defn consume-record-header
  [[line & more-lines]]
  (if-let [data (tokenize line ["RECORD BEGIN TIMESTAMP " :timestamp "."])]
    [more-lines data]
    (throw (ex-info "Record header incorrect"
                    {:reason :record-header-incorrect
                     :line   line}))))

(defn consume-game-line
  [[line & more-lines]]
  [more-lines (tokenize line ["Game is " :game-type " type " :network-type])])

(defn consume-mission-line
  [[line & more-lines :as lines]]
  (if-let [data (tokenize line ["Mission name: " :mission-name])]
    [more-lines data]
    [lines nil]))

(defn consume-blank-lines
  [lines]
  [(drop-while str/blank? lines)])

(defn consume-game-header
  [lines]
  (consume lines
           merge
           [consume-game-line
            consume-mission-line
            consume-blank-lines]))

(defn consume-mission
  [lines]
  (consume lines
           merge
           [(tokenized-line ["Mission Type: " :mission-type])
            (tokenized-line ["Flight Unique Id: " :flight-unique-id])
            (tokenized-line [:flight-size " Ship Flight"])
            (tokenized-line ["Ac type: " :ac-type])
            (tokenized-line ["Contry: " :country])
            consume-blank-lines]))

(defn consume+
  "Returns a consumer that consumes one or more of whatever consumer
  consumes. Returns a vector with the produced data."
  [consumer]
  (fn [lines]
    (let [results (->> [lines]
                    (iterate #(consumer (first %)))
                    (drop 1)
                    (take-while #(some? (second %))))]
      [(first (last results))
       (mapv second results)])))

(defn ->map
  "Returns a consumer that consumes whatever consumer does, but wraps
  the result in a one-element map with key k."
  [k consumer]
  (fn [lines]
    (let [[lines data] (consumer lines)]
      [lines {k data}])))

(def flight-event-actions
  #{"launched at"
    "joined as"
    "exited from"
    "ejected"
    "landed"
    "downed by"
    "destroyed by"
    "entered"
    "left"})

(defn alternative-consumer
  "Returns a consumer that will return the results of the first
  consumer that returns non-nil data."
  [alternatives]
  (fn [lines]
    (or (->> alternatives
          (map (fn [alternative] (alternative lines)))
          (filter #(some? (second %)))
          first)
        [lines])))

;; I think this is going to have to get more sophisticated. Good thing
;; we're in the land of code.
(def consume-flight-event
  (alternative-consumer
   (mapv #(tokenized-line ["Event " :object [:action %] " " :subject])
         flight-event-actions)))

(defn consume-flight-events
  [lines]
  (consume lines
           merge
           [(tokenized-line ["FLIGHT EVENTS"])
            (->map :flight-events (consume+ consume-flight-event))
            consume-blank-lines]))

(defn consume-weapon-data
  [lines]
  [lines])

(def number #(Long. %))

(defn consume-pilot-block
  [lines]
  (consume lines
           merge
           [(tokenized-line ["-------------"])
            (tokenized-line ["PILOT SLOT " [:pilot-number number] ":"])
            consume-blank-lines
            (tokenized-line ["Human Player: " :player-name])
            (tokenized-line ["Callsign: " :callsign])
            (tokenized-line ["Pilot status  - " :pilot-status])
            (tokenized-line ["Aircraft status  - " :aircraft-status])
            (tokenized-line ["AA Kills " [:aa-kills number]])
            (tokenized-line ["AG Kills " [:ag-kills number]])
            (tokenized-line ["AS Kills " [:as-kills number]])
            (tokenized-line ["AN Kills " [:an-kills number]])
            (tokenized-line ["Shoot At " [:shoot-at number]])
            (tokenized-line ["Other Player Kills " [:other-player-kills number]])
            consume-blank-lines
            (->map :weapon-data consume-weapon-data)]))

(defn consume-pilot-data
  [lines]
  (consume+ consume-pilot-block))

(defn consume-map
  [lines & pairs]
  (loop [[[k f] & more] (partition 2 pairs)
         lines lines
         m {}]
    (if-not k
      [lines m]
      (let [[lines data] (f lines)]
       (recur more lines (assoc m k data))))))

(defn consume-record-data
  [lines]
  (consume-map lines
               :game-info consume-game-header
               :mission consume-mission
               :flight-events consume-flight-events
               :pilot-data (consume+ consume-pilot-data)))

(defn consume-record
  [lines]
  (consume lines merge consume-record-header consume-record-data))

(defn consume-section
  [lines]
  (when-not (empty? lines)
    (let [[separator & more-lines] lines]
      (when-not (separator? separator)
        (throw (ex-info "Should have been separator"
                        {:reason :not-separator
                         :line separator})))
      (let [[lines record] (consume-record more-lines)]
        [lines record]))))


(defn parse
  [debrief]
  (let [lines (->> debrief
                str/split-lines
                (map str/trim)
                (drop-while str/blank?))]
    (->> [lines]
      (iterate #(consume-section (first %)))
      (mapv second)
      (drop 1)
      (take-while some?))))
