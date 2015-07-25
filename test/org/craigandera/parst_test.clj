(ns org.craigandera.parst_test
  (:require [clojure.test :refer :all]
            [org.craigandera.parst :as p]))

(deftest tokenize
  (are [input tokens output]
    (is (= (p/tokenize input tokens) output))
    "hi" ["hi"] {}
    "hi" [] {}
    "no" ["hi"] nil

    "hi there"
    ["hi " :name]
    {:name "there"}

    "hi there"
    [[:greeting "hi"] " " :name]
    {:greeting "hi"
     :name "there"}

    "hi there 17"
    ["hi there " [:number (fn [val] (Long. val))]]
    {:number 17}))

(deftest consume*
  (are [input consumer unconsumed parsed]
    (let [consumer* (p/consume* consumer)
          result (consumer* input)]
      (is (= unconsumed (p/unconsumed result)))
      (is (= parsed (p/parsed result))))

    ["a" "b" "c"]
    (p/tokenizer ["a"])
    ["b" "c"]
    [{}]

    ["a" "b" "c"]
    (p/tokenizer [[:a "a"]])
    ["b" "c"]
    [{:a "a"}]

    ["a" "a" "b"]
    (p/tokenizer [[:a "a"]])
    ["b"]
    [{:a "a"}
     {:a "a"}]

    ["a" "a" "b"]
    (p/tokenizer ["b"])
    ["a" "a" "b"]
    []))

(deftest consume+
  (are [input consumer unconsumed parsed]
    (let [consumer+ (p/consume+ consumer)
          result (consumer+ input)]
      (is (= unconsumed (p/unconsumed result)))
      (is (= parsed (p/parsed result))))

    ["a" "a" "b"]
    (p/tokenizer [[:a "a"]])
    ["b"]
    [{:a "a"}
     {:a "a"}]

    ["a" "b" "c"]
    (p/tokenizer [[:a "a"]])
    ["b" "c"]
    [{:a "a"}])

    ["a" "a" "b"]
    (p/tokenizer ["b"])
    ["a" "a" "b"]
    nil)

(deftest consume-all
  (are [input consumers unconsumed parsed]
    (let [result (p/consume-all input merge consumers)]
      (is (= (seq unconsumed) (seq (p/unconsumed result))))
      (is (= parsed (p/parsed result))))

    ["a"]
    [(p/tokenizer [[:a "a"]])]
    nil
    {:a "a"}

    ["a" "b" "c"]
    [(p/tokenizer [[:a "a"]])
     (p/tokenizer [[:b "b"]])]
    ["c"]
    {:a "a"
     :b "b"}))

(deftest consume-map
  (let [consume-one (fn [strings] (p/->result (rest strings) (first strings)))]
    (are [input consumers unconsumed parsed]
      (let [result (apply p/consume-map input consumers)]
        (is (= (seq unconsumed) (seq (p/unconsumed result))))
        (is (= parsed (p/parsed result))))

      ["a" "b" "c"]
      [:a consume-one
       :b consume-one]
      ["c"]
      {:a "a"
       :b "b"}

      ["a" "b" "c"]
      [:a consume-one
       :b (p/tokenizer ["no match"])
       :c consume-one]
      ["a" "b" "c"]
      nil)))
