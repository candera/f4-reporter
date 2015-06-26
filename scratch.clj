(def test-rules
  (insta/parser "file = section*
   section =
   line = line-content? newline
   line-content =
   newline = '\n'"))

(def test-rules
  (-> "grammar.txt" slurp insta/parser))

(-> "\n\n-----\nRECORD BEGIN TIMESTAMP 12/11/2014 09:03:40.\n" test-rules)

(require '[instaparse.core :as insta])
(require '[clojure.pprint :refer [pprint]])
(require '[clojure.string :as str])
(-> "grammar.txt"
  slurp
  insta/parser
  ;;(.invoke "\n\n-----\nRECORD BEGIN TIMESTAMP 12/11/2014 09:03:40.\n")
  (insta/parse (slurp "debrief-short.txt"))
  ;;insta/get-failure
  )

(-> "grammar.txt"
  slurp
  insta/parser
  (insta/parse "Event DPRK VTT-323 destroyed by Tyrant at 11:06:05
" #_"Event GSH-301 launched at Tyrant at 09:00:43
" :start-at :flight-event)
  ;;insta/get-failure
  ;;:text
  )

(-> "flight-event =
  'Event' whitespace-char+
  object whitespace-char+
  action whitespace-char+
  (subject whitespace-char+)?
  ('at' whitespace-char+)?
  time?
object = words
action =
 'launched at' |
 'joined as' |
 'exited from' |
 'ejected' |
 'landed' |
 'destroyed by'
subject = words
<whitespace-char> = ' ' | '\\t'
<words> = (#'\\p{Graph}+' whitespace-char*)+
time = hour <':'> minute <':'> second
hour = two-digit-number
minute = two-digit-number
second = two-digit-number
<two-digit-number> = #'\\d{2}'
"
  insta/parser
  (insta/parse "Event DPRK VTT-323 destroyed by Tyrant at 11:06:05"))


(re-matches #"\n" "")

(-> "line = 'foo' #'.*$' '\n'"
  insta/parser
  (insta/parse "foo bar\n" :partial true))

(str/replace (slurp "debrief-short.txt") "\012" "")

(def debrief
  (-> "grammar.txt"
    slurp
    insta/parser
    (insta/parse (slurp "debrief.txt"))))

(insta/failure? debrief)

(first debrief)


(def x
  (->> debrief
    (take 21)
    (mapv mapify)
    last
    ))

x

(last x)

(->> debrief
  (filter (fn [tag & record-contents] (some (fn [[tag contents]]
                                              (and (= tag :mission-name)
                                                   (= contents "-33-Refueling")))
                                            record-contents)))
  count)

(count debrief)

(for [[_ & record-contents] debrief
      [tag & flight-events] record-contents :when (= tag :flight-events)
      [_ [_ object] [_ action] [_ subject] [_ & time]] flight-events]
  {:object object
   :action action
   :subject subject
   :time time})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def data
  (->> "debrief-short.txt"
    org.craigandera.debrief-parser/parse))

(def raw
  (->> "debrief-short.txt"
    org.craigandera.debrief-parser/parse*))

(first raw)

(org.craigandera.debrief-parser/mapify (first raw))
(org.craigandera.debrief-parser/->map [[:timestamp-date [:month "12"] [:day "11"] [:year "2014"]] [:timestamp-time [:hour "09"] [:minute "03"] [:second "40"]]])

(->> data
  (remove :mission-name)
  (map :game-type))

(-> data
  first
  (org.craigandera.debrief-parser/mission-time "Tyrant"))

(->> data
  (filter #(or (empty? (get-in % [:timestamp :date :year]))
               (empty? (get-in % [:timestamp :date :month]))
               (empty? (get-in % [:timestamp :date :day]))
               (empty? (get-in % [:timestamp :time :hour]))
               (empty? (get-in % [:timestamp :time :minute]))
               (empty? (get-in % [:timestamp :time :second]))))
  count)

(->> d
  (mapv p))

(def d2
  (->> d
    (mapv (fn [record]
            (let [mission-duration (mission-time record "Tyrant")]
              (assoc record
                     :mission-time mission-duration
                     :mission-pretty-hours (when mission-duration
                                             (.print (org.joda.time.format.PeriodFormat/getDefault)
                                                     (.toPeriod mission-duration)))))))))
(->> d2
  (map :mission-time)
  (remove nil?)
  (map (fn [t] (-> t .toStandardSeconds .getSeconds (/ 3600.0))))
  (reduce +))

(require '[instaparse.core :as insta])
(require '[clojure.java.io :as io])
(-> "grammar.txt" io/resource slurp insta/parser (insta/parse (slurp "debrief-short.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def debrief (->> "debrief-short.txt"
               slurp
               str/split-lines
               (map str/trim)
               (drop-while str/blank?)))

(consume-section debrief)

(consume-mission-line ["Mission name: foobar" ""])

(tokenize "Mission name: foobar" "Mission name: " :mission-name)

(consume-record-header ["RECORD BEGIN TIMESTAMP 12/04/22 01:02:03." "more" "and more"])

(consume-game-line ["Game is Tactical Engagement type Local" "more" "and more"])

(consume-mission-line [;;"Mission name: -01-Basic Handling"
                       ""
                       "Mission Type: DCA"])

(consume-blank-lines ["" " " "" "b" "c"])

(consume-game-header [;; "RECORD BEGIN TIMESTAMP 12/11/2014 09:03:40."
                      "Game is Tactical Engagement type Local"
                      "Mission name: -01-Basic Handling"
                      ""
                      "Mission Type: DCA"
                      "Flight Unique Id: Cowboy1"
                      "1 Ship Flight"
                      "Ac type: F-16D-52"
                      "Contry: U.S."])

(consume-mission [ ;; "RECORD BEGIN TIMESTAMP 12/11/2014 09:03:40."
                  ;;"Game is Tactical Engagement type Local"
                  ;;"Mission name: -01-Basic Handling"
                  ;;""
                  "Mission Type: DCA"
                  "Flight Unique Id: Cowboy1"
                  "1 Ship Flight"
                  "Ac type: F-16D-52"
                  "Contry: U.S."
                  ""
                  "FLIGHT EVENTS"
                  "Event Tyrant joined as Cowboy11 at 12:01:21"
                  "Event 30mm GSH-301 launched at Tyrant 12:02:32"
                  "Event 30mm GSH-301 launched at Tyrant 12:02:32"
                  "Event Tyrant exited from Cowboy11 at 12:07:06"
                  ""
                  "-------------"])

(consume-flight-events [ ;; "RECORD BEGIN TIMESTAMP 12/11/2014 09:03:40."
                        ;;"Game is Tactical Engagement type Local"
                        ;;"Mission name: -01-Basic Handling"
                        ;;""
                        ;; "Mission Type: DCA"
                        ;; "Flight Unique Id: Cowboy1"
                        ;; "1 Ship Flight"
                        ;; "Ac type: F-16D-52"
                        ;; "Contry: U.S."
                        ;; ""
                        "FLIGHT EVENTS"
                        "Event Tyrant joined as Cowboy11 at 12:01:21"
                        "Event 30mm GSH-301 launched at Tyrant 12:02:32"
                        "Event 30mm GSH-301 launched at Tyrant 12:02:32"
                        "Event Tyrant exited from Cowboy11 at 12:07:06"
                        ""
                        "-------------"])

(parser/consume-pilot-block ["-------------"
                             "PILOT SLOT 1:"
                             ""
                             "Human Player: 2Lt. Craig Andera"
                             "Callsign: Tyrant"
                             "Pilot status  - OK"
                             "Aircraft status  - OK"
                             "AA Kills 0"
                             "AG Kills 0"
                             "AS Kills 0"
                             "AN Kills 0"
                             "Shoot At 2"
                             "Other Player Kills 0"
                             ""
                             ""])

(tokenize "Event 30mm GSH-301 launched at Tyrant 12:02:32"
          ["Event " :object [:action "launched at"] " " :subject])

(tokenize "AA Kills 0" ["AA Kills " [:kills#(Long. %)]])


