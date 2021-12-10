(ns aoc.dec10
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn input []
  (utils/day-file 10))

(def open? #{\[ \{ \( \<})
(def close? #{\] \} \) \>})

(def mapping
  {\[ \]
   \{ \}
   \( \)
   \< \>})

(def score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn parse-line [line]
  (loop [open []
         [ch & more] (seq line)]
    (cond
      (nil? ch) (if (empty? open)
                  true
                  open)

      (open? ch) (recur (conj open ch) more)

      (close? ch)
      (if (= (mapping (last open)) ch)
        (recur (pop open) more)
        ch))))

(defn part1 []
  (let [data (input)]
    (->> (for [line data
               :let [res (parse-line line)]
               :when (char? res)]
           (score res))
         (reduce +))))

(def part2-score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn part2 []
  (let [data (input)
        scores (->> (for [line data
                          :let [res (parse-line line)]
                          :when (vector? res)]
                      (->> res
                           (reverse)
                           (map mapping)
                           (reduce
                             (fn [total ch]
                               (-> total (* 5) (+ (part2-score ch))))
                             0)))
                    (sort))]
    (nth scores (/ (count scores) 2))))
