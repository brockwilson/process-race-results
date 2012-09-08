(ns process-race-results.core
  (:use [clojure.string :only [split-lines split trim lower-case join]])
  (:use [clojure.set :only [intersection]])
  (:use [clojure-csv.core ])
  (:use [clojure.math.numeric-tower :only [expt floor]])
  (:use (incanter core stats charts)))


(defn split-line-by-indexes-list [line indexes-list]
  (for [indexes indexes-list]
    (clojure.string/trim (subs line (first indexes) (second indexes)))))

(defn convert-to-time-in-seconds [string]
  (if (not (= (.indexOf string ":") -1))
    (let [split-time (clojure.string/split string #":")]
      (try (reduce +
                   (map-indexed (fn [index value]
                                  (* (expt 60 index) (Integer/parseInt value)))
                                (reverse split-time)))
           (catch NumberFormatException e
             string)))
    string))

(defn recursive-convert-time-in-seconds-to-string [time-in-seconds exponent accumulator]
  (if (= exponent -1)
    accumulator
    (let [time-in-exponent (floor (/ time-in-seconds
                                     (expt 60 exponent)))]
      (recur (- time-in-seconds (* time-in-exponent
                                   (expt 60 exponent)))
             (- exponent 1)
             (str accumulator
                  (if (= "" accumulator)
                    ""
                    ":")
                  (if (< time-in-exponent 10)
                    (str "0" time-in-exponent)
                    (str time-in-exponent)))))))

(defn convert-time-in-seconds-to-string [time-in-seconds]
  (recursive-convert-time-in-seconds-to-string time-in-seconds 2 ""))

       



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

(defn process-rows-by-common-whitespace [rows]
  (let [data-indexes (find-data-indexes rows)]
    (map #(split-line-by-indexes-list % data-indexes) rows)))

(defn process-file-by-whitespace [file-name]
  (process-rows-by-common-whitespace (clojure.string/split-lines (slurp file-name))))

(defn process-file-by-delimiter [file-name delimiter]
  (let [data (clojure-csv.core/parse-csv (slurp file-name) :delimiter delimiter)]
    (map (fn [row] (map (fn [row-entry] (convert-to-time-in-seconds row-entry)) row)) data)))

;; data structures come out slightly different in each case.
(defn process-file [file-name delimiter]
    (if (= delimiter :whitespace)
      (process-file-by-whitespace file-name)
      (process-file-by-delimiter file-name delimiter)))




    


  
(defn calculate-percentile [datum data]
  (/ (.indexOf (sort data)
               datum)
     (length data)
     1.0))


;; Sinister 7 specific functions

(defn extract-single-leg-datum [row leg-number]
  (try (- (nth row (+ leg-number 4))
          (if (= leg-number 1)
            0
            (nth row (+ leg-number 3))))
       (catch ClassCastException e
         false)))

(defn extract-single-leg-data [data leg-number]
  (let [proc-func #(extract-single-leg-datum % leg-number)
        filtered-data (filter proc-func data)]
    (map proc-func filtered-data)))

(defn process-sinister-7-results [file-name team-name]
  (let [data (process-file file-name \tab)
        team-details (first (filter (fn [x]
                                      (= (clojure.string/lower-case team-name)
                                         (clojure.string/lower-case (nth x 2))))
                                    data))]
    (map (fn [x] (let [leg-data (extract-single-leg-data data x)]
                   (view (histogram (map #(/ % 3600.0) leg-data)
                                    :nbins 20
                                    :x-label "Time (hr)"
                                    :y-label "Counts"
                                    :title (format "Leg %d" x)))
                   [:leg x
                    :time (convert-time-in-seconds-to-string (extract-single-leg-datum team-details x))
                    :percentile (* 100
                                   (calculate-percentile (extract-single-leg-datum team-details x)
                                                         leg-data))]))
         (range 1 8))))