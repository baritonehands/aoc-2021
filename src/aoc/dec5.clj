(ns aoc.dec5
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn horizontal? [[_ fy] [_ ty]]
  (= fy ty))

(defn vertical? [[fx _] [tx _]]
  (= fx tx))

(defn input []
  (for [[from to] (->> (utils/day-file 5)
                       (map #(str/split % #" -> ")))]
    [(mapv #(Integer/parseInt %) (str/split from #","))
     (mapv #(Integer/parseInt %) (str/split to #","))]))


(defn abs-range [from to]
  (if (> to from)
    (range from (inc to))
    (range to (inc from))))

(defn part1 []
  (let [data (->> (input)
                  (filterv (fn [[from to]]
                             (or (horizontal? from to) (vertical? from to)))))]
    (->> (for [[[fx fy :as from] [tx ty :as to]] data]
           (if (horizontal? from to)
             (for [dx (abs-range fx tx)]
               [dx fy])
             (for [dy (abs-range fy ty)]
               [fx dy])))
         (mapcat identity)
         (frequencies)
         (filter #(> (second %) 1))
         (count))))

(defn part2 []
  (let [data (input)]
    (->> (for [[[fx fy :as from] [tx ty :as to]] data
               :let [xdir (if (> tx fx) 1 -1)
                     ydir (if (> ty fy) 1 -1)]]
           (cond
             (horizontal? from to)
             (for [dx (abs-range fx tx)]
               [dx fy])

             (vertical? from to)
             (for [dy (abs-range fy ty)]
               [fx dy])

             :else
             (for [di (range 0 (inc (Math/abs (- fx tx))))]
               [(+ fx (* di xdir))
                (+ fy (* di ydir))])))
         (mapcat identity)
         (frequencies)
         (filter #(> (second %) 1))
         (count))))

