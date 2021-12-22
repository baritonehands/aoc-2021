(ns aoc.dec21
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn input []
  (->> (utils/day-file 21)
       (mapv (comp #(Long/parseLong %) second #(str/split % #": ")))))

(defn add-roll [pos turn]
  (let [pos (mod (+ pos turn) 10)]
    (if (zero? pos) 10 pos)))

(defn run-game [[p1 p2]]
  (let [rolls (cycle (range 1 101))]
    (loop [[turn more] (split-at 3 rolls)
           player 0
           scores [0 0]
           pos [p1 p2]
           cnt 0]
      (if (some #(>= % 1000) scores)
        [cnt (sort-by first (map-indexed vector scores))]
        (let [next-pos (-> (get pos player)
                           (add-roll (reduce + turn)))]
          (println [turn player scores pos next-pos])
          (recur
            (split-at 3 more)
            (if (zero? player) 1 0)
            (update scores player + next-pos)
            (assoc pos player next-pos)
            (+ cnt 3)))))))

(defn part1 []
  (let [data (input)]
    (run-game data)))

(defn run-game2 [[p1 p2]]
  (let [rolls (range 1 4)]
    (loop [[turn more] (split-at 3 rolls)
           player 0
           scores [0 0]
           pos [p1 p2]
           cnt 0]
      (if (some #(>= % 1000) scores)
        [cnt (sort-by first (map-indexed vector scores))]
        (let [next-pos (-> (get pos player)
                           (add-roll (reduce + turn)))]
          (println [turn player scores pos next-pos])
          (recur
            (split-at 3 more)
            (if (zero? player) 1 0)
            (update scores player + next-pos)
            (assoc pos player next-pos)
            (+ cnt 3)))))))

(defn part2 [])
