(ns process-race-results.core
  (:use [clojure.string :only [split-lines]]))

(defn return-columns [filename]
  (clojure.string/split-lines (slurp filename)))