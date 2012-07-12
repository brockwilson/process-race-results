(ns process-race-results.core
  (:use [clojure.string :only [split-lines split trim]])
  (:use [clojure.set :only [intersection]])
  (:use [clojure-csv.core :only [write-csv]])
  (:use [clojure.math.numeric-tower :only [expt]]))



(defn split-file-to-rows [file-name]
  (clojure.string/split-lines (slurp file-name)))

(defn find-char-in-string [string char]
  (map first
       (filter #(= (second %) char)
               (map-indexed vector string))))

(defn find-common-whitespace [list-of-strings]
  (sort (seq (reduce clojure.set/intersection
                     (map (fn [x] (set (find-char-in-string x \space)))
                          list-of-strings)))))

(defn find-data-indexes [list-of-strings]
  (let [common-white-space (find-common-whitespace list-of-strings)]
    (for [[x y] (apply map vector [common-white-space (rest common-white-space)])
	 :when (> (- y x) 1)]
	[x y])))

(defn split-line-by-indexes-list [line indexes-list]
  (for [indexes indexes-list]
    (subs line (first indexes) (second indexes))))

(defn convert-to-time-in-seconds [string]
  ;; need to strip out white space
  (if (not (= (.indexOf string ":") -1))
    (let [split-time (clojure.string/split string #":")]
      (reduce + 
              (map-indexed (fn [index value]
                             (println value)
                             (* (clojure.math.numeric-tower/expt 60 index)
                                (Integer/parseInt value)))
                           (reverse split-time))))
    string))

(defn do-everything [file-name]
  (let [file-as-list-of-strings (split-file-to-rows file-name)
        data-indexes (find-data-indexes file-as-list-of-strings)
        split-data (map (fn [string]
                          (map convert-to-time-in-seconds
                               (split-line-by-indexes-list string data-indexes)))
                        file-as-list-of-strings)
        csv-string (clojure-csv.core/write-csv split-data)
        csv-filename (str file-name ".csv")]
    (spit csv-filename csv-string)))

;; (let [string "1:2:3"]
;; 				(if (not (= (.indexOf string ":") -1))
;; 				    (let [split-time (split string #":")]
;; 					 (map-indexed (fn [index value]
;; 							  (+ (