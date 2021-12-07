(ns aoc.dec7
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn input []
  (mapv
    #(Integer/parseInt %)
    (-> (utils/day-file 7)
        (first)
        (str/split #","))))

(defn part1 []
  (let [data (input)
        xmin (apply min data)
        xmax (apply max data)]
    (->> (for [x (range xmin xmax)]
           [x (->> (for [pos data]
                     (Math/abs ^long (- x pos)))
                   (reduce +))])
         (sort-by second)
         (first))))

(defn bowling [n]
  (/ (* n (inc n)) 2))

(defn part2 []
  (let [data (input)
        xmin (apply min data)
        xmax (apply max data)]
    (->> (for [x (range xmin xmax)]
           [x (->> (for [pos data]
                     (bowling (Math/abs ^long (- x pos))))
                   (reduce +))])
         (sort-by second)
         (first))))
