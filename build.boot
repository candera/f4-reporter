(set-env!
 :source-paths #{"src" "test"}
 :resource-paths #{"resources"}
 :target-path "target"
 :dependencies '[ ;; boot
                 [adzerk/boot-cljs "0.0-3308-0" :scope "test"]
                 [adzerk/boot-test "1.0.4" :scope "test"]
                 ;;[adzerk/boot-reload "0.2.4" :scope "test"]

                 ;; Clojure
                 [org.clojure/clojure "1.7.0-RC2"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [instaparse "1.4.0"]
                 [joda-time "2.8"]

                 ;; ClojureScript
                 [org.clojure/clojurescript "0.0-3308"]
                 ;;[com.lucasbradstreet/instaparse-cljs "1.4.0.0-SNAPSHOT"]
                 ])

(require
 '[adzerk.boot-cljs :refer [cljs]]
 ;;'[adzerk.boot-reload :refer [reload]]
 '[clojure.tools.namespace.repl :as repl :refer [refresh]]
 '[org.craigandera.f4-reporter.parser :as dp]
 '[org.craigandera.parst :as parst]
 '[clojure.string :as str]
 '[adzerk.boot-test :refer :all]
 )

(apply repl/set-refresh-dirs (get-env :directories))


(task-options!
  pom {:project 'org.craigandera/f4-reporter
       :version "0.1.0"}
  jar {:manifest {"author" "Craig Andera"}}
  cljs {:main 'org.craigandera.f4-reporter.main
        :asset-path "target/out"
        :optimizations :simple
        :source-map true})

