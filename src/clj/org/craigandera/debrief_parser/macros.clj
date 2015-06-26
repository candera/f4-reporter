(ns org.craigandera.debrief-parser.macros
  "Macros"
  (:require [clojure.java.io :as io]))

(defmacro slurp-resource
  [path]
  (-> path io/resource slurp))
