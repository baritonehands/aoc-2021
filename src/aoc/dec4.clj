(ns aoc.dec4
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn board-input [board]
  (vec
    (for [row board]
      (->> (str/split (.trim row) #"\s+")
           (mapv #(Integer/parseInt %))))))

(defn input []
  (let [[called & boards] (utils/day-file 4)]
    [(->> (str/split called #",")
          (mapv #(Integer/parseInt %)))
     (->> boards
          (partition-by #(= % ""))
          (remove #(= (count %) 1))
          (mapv board-input))]))

(defn seq-winner? [called xs]
  (some true? (for [row xs]
                (every? called row))))

(defn winner? [called board]
  (or
    (seq-winner? called board)
    (seq-winner?
      called
      (for [idx (range 0 (count board))]
        (mapv #(get % idx) board)))))

(defn board-score [called board]
  (->> board
       (mapcat identity)
       (remove called)
       (reduce +)))

(defn part1 []
  (let [[numbers boards] (input)]
    (loop [called #{}
           [cur & more] numbers]
      (let [next-called (conj called cur)
            winner (some
                     (fn [board]
                       (and
                         (winner? next-called board)
                         board))
                     boards)]
        (if winner
          (* (board-score next-called winner) cur)
          (recur next-called more))))))

(defn part2 []
  (let [[numbers boards] (input)]
    (loop [called #{}
           [cur & more] numbers
           remaining boards]
      (let [next-called (conj called cur)
            next-remaining (remove #(winner? next-called %) remaining)]
        (if (empty? next-remaining)
          (* (board-score next-called (first remaining)) cur)
          (recur next-called more next-remaining))))))
