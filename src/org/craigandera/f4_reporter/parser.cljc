(ns org.craigandera.f4-reporter.parser
  "A parser for Falcon debrief files"
  (:require [clojure.string :as str]
            [org.craigandera.parst :as p]))

(def consume-record-header
  (p/tokenizer ["RECORD BEGIN TIMESTAMP " :timestamp "."]))

(defn consume-game-string
  [[string & more-strings]]
  [more-strings (p/tokenize string ["Game is " :game-type " type " :network-type])])

(defn consume-mission-string
  [[string & more-strings :as strings]]
  (if-let [data (p/tokenize string ["Mission name: " :mission-name])]
    [more-strings data]
    [strings {}]))

(defn blank-consumer
  "Returns a consumer that consumes zero or more blank strings and
  parses to val."
  [val]
  (fn [strings]
    [(drop-while str/blank? strings)
     val]))

(defn consume-game-header
  [strings]
  (p/consume-all strings
                 merge
                 [consume-game-string
                  consume-mission-string
                  (blank-consumer {})]))

(defn consume-mission
  [strings]
  (p/consume-all strings
                 merge
                 [(p/tokenizer ["Mission Type: " :mission-type])
                  (p/tokenizer ["Flight Unique Id: " :flight-unique-id])
                  (p/tokenizer [:flight-size " Ship Flight"])
                  (p/tokenizer ["Ac type: " :ac-type])
                  (p/tokenizer ["Contry: " :country])
                  (blank-consumer {})]))

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

;; I think this is going to have to get more sophisticated. Good thing
;; we're in the land of code.
(def consume-flight-event
  (p/alternative-consumer
   (mapv #(p/tokenizer ["Event" [:object str/trim] [:action %] [:subject str/trim]])
         flight-event-actions)))

(defn consume-flight-events
  [strings]
  (p/transform :flight-events
               (p/consume-map
                strings
                :header (p/tokenizer ["FLIGHT EVENTS"])
                :flight-events (p/consume+ consume-flight-event)
                :blanks (blank-consumer :ignore))))

(def number #(Long. ^String %))

(def consume-weapon-event
  (p/alternative-consumer
   (let [effects [[[:result "miss"]]
                  [[:result "hit"] " " :target " - " :damage]]
         style1 (for [employment [["Event " :weapon " released at " :timestamp]
                                  ["Event " :weapon " fired at " :timestamp]
                                  ["Event "]]
                      effect effects]
                  (concat employment ["@72"] effect))
         style2 (for [effect effects]
                  (into ["Event " :pilot " released " :weapon ": "]
                        effect))]
     (map p/tokenizer (concat style1 style2)))))

(defn consume-loadout
  [strings]
  (p/consume-all strings
                 merge
                 [(p/tokenizer ["LOADOUT " [:number number] ": " :weapon])
                  (p/tokenizer ["Starting Load " [:starting-load number]])
                  (p/tokenizer ["Fired " [:fired number]])
                  (p/tokenizer ["Missed " [:missed number]])
                  (p/tokenizer ["Hit " [:hit number]])
                  (p/->map :events (p/consume* consume-weapon-event))
                  (blank-consumer {})]))

(defn consume-weapon-data
  [strings]
  (p/consume-all strings
                 merge
                 [(p/tokenizer ["WEAPON DATA"])
                  (blank-consumer {})
                  (p/->map :loadouts (p/consume+ consume-loadout))]))

(defn consume-pilot-block
  [strings]
  (p/consume-all strings
                 merge
                 [(p/tokenizer ["-------------"])
                  (p/tokenizer ["PILOT SLOT " [:pilot-number number] ":"])
                  (blank-consumer {})
                  (p/tokenizer ["Human Player: " :player-name])
                  (p/tokenizer ["Callsign: " :callsign])
                  (p/tokenizer ["Pilot status  - " :pilot-status])
                  (p/tokenizer ["Aircraft status  - " :aircraft-status])
                  (p/tokenizer ["AA Kills " [:aa-kills number]])
                  (p/tokenizer ["AG Kills " [:ag-kills number]])
                  (p/tokenizer ["AS Kills " [:as-kills number]])
                  (p/tokenizer ["AN Kills " [:an-kills number]])
                  (p/tokenizer ["Shoot At " [:shoot-at number]])
                  (p/tokenizer ["Other Player Kills " [:other-player-kills number]])
                  (blank-consumer {})
                  (p/->map :weapon-data consume-weapon-data)]))

(defn consume-record-data
  [strings]
  (p/consume-map strings
                 :game-info consume-game-header
                 :mission consume-mission
                 :flight-events consume-flight-events
                 :pilot-data (p/consume+ consume-pilot-block)))

(defn consume-record
  [strings]
  (p/consume-all strings
                 merge
                 [consume-record-header
                  consume-record-data]))

(defn consume-section
  [strings]
  (p/transform :record
               (p/consume-map
                strings
                :leading-blanks (blank-consumer :ignored)
                :separator (p/tokenizer ["----------------------"])
                :record consume-record
                :trailing-blanks (blank-consumer :ignored))))

(defn parse
  [debrief]
  (let [lines (->> debrief
                str/split-lines
                (map str/trim))
        consumer (p/consume+ consume-section)
        [unconsumed parsed] (consumer lines)]
    (when-not (empty? unconsumed)
      (throw (ex-info "Unparsed content remains in the file"
                      {:reason :leftovers
                       :consumed (count parsed)
                       :remainder (take 20 unconsumed)})))
    parsed))
