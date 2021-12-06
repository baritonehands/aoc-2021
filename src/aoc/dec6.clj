(ns aoc.dec6
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn input []
  (mapv #(Integer/parseInt %)
        (-> (utils/day-file 6)
            (first)
            (str/split #","))))

(defn generation [cur]
  (->> (reduce
         (fn [next-gen fish]
           (if (zero? fish)
             (-> next-gen
                 (update 0 conj 6)
                 (update 1 conj 8))
             (update next-gen 0 conj (dec fish))))
         [[] []]
         cur)
       (apply into)))

(defn generation2 [cur]
  (let [zero (get cur 0 0)
        result (->> (for [[fish n] cur
                          :when (pos? fish)]
                      [(dec fish) n])
                    (into {}))]
    (-> result
        (update 6 (fnil + 0) zero)
        (update 8 (fnil + 0) zero))))

(defn part1 []
  (let [data (frequencies (input))]
    (loop [cur data
           n 0]
      (if (= n 80)
        cur
        (recur (generation2 cur) (inc n))))))

(defn part2 []
  (let [data (frequencies (input))]
    (loop [cur data
           n 0]
      (if (= n 256)
        cur
        (recur (generation2 cur) (inc n))))))
