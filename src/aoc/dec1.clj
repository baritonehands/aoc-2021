(ns aoc.dec1
  (:require [aoc.utils :as utils]))

(defn input []
  (->> (utils/day-file 1)
     (mapv #(Integer/parseInt %))))

(defn run
  "I don't do a whole lot."
  [data]
  (reduce
    (fn [{:keys [cnt last] :as acc} n]

      {:cnt  (cond
               (nil? last) 0
               (> n last) (inc cnt)
               :else cnt)
       :last n})
    {:cnt  0
     :last nil}
    data))

(defn part1 []
  (run (input)))

(defn part2 []
  (let [data (input)]
    (->> (for [n (range 0 (count data))
               :when (< n (- (count data) 2))]
           (->> data
                (drop n)
                (take 3)))
         (map #(apply + %))
         (run))))

