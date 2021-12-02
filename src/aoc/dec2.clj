(ns aoc.dec2
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(defn input []
  (->> (utils/day-file 2)
       (mapv
         (fn [row]
           (let [[dir n] (str/split row #" ")]
             [(keyword dir) (Integer/parseInt n)])))))

(defn part1 []
  (let [data (input)]
    (->> (reduce
           (fn [acc [dir n]]
             (case dir
               :forward (update acc :pos #(+ % n))
               :down (update acc :depth #(+ % n))
               :up (update acc :depth #(- % n))))
           {:pos   0
            :depth 0}
           data)
         (vals)
         (apply *))))

(defn part2 []
  (let [data (input)]
    (-> (reduce
          (fn [acc [dir n]]
            (case dir
              :forward (-> acc
                           (update :pos #(+ % n))
                           (update :depth #(+ % (* (:aim acc) n))))
              :down (update acc :aim #(+ % n))
              :up (update acc :aim #(- % n))))
          {:pos   0
           :depth 0
           :aim   0}
          data)
        (select-keys [:pos :depth])
        (vals)
        (->> (apply *)))))
