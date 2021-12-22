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

(def dirac-die
  (->> (for [i (map inc (range 0 3))
             j (map inc (range 0 3))
             k (map inc (range 0 3))]
         (+ i j k))
       (frequencies)))

(defn play-turn [{:keys [player scores pos]} roll]
  (let [next-pos (-> (get pos player)
                     (add-roll roll))]
    {:player (if (zero? player) 1 0)
     :scores (update scores player + next-pos)
     :pos (assoc pos player next-pos)}))

(defn run-game2 [as-player [p1 p2]]
  (let [states (atom {})]
    (letfn [(win? [{[score1 score2] :scores}]
              (cond
                (>= score1 21) 0
                (>= score2 21) 1
                :else false))
            (recurse [game n]
              (cond
                (win? game) (if (= as-player (win? game)) n 0)

                (contains? @states game)
                (* n (get @states game))

                :else
                (let [wins (->> (for [[throw cnt] dirac-die]
                                  [(play-turn game throw) cnt])
                                (reduce
                                  (fn [wins [new-game cnt]]
                                    (+ wins (recurse new-game cnt)))
                                  0))]
                  (swap! states assoc game wins)
                  (* wins n))))]
      (recurse {:player 0
                :scores [0 0]
                :pos [p1 p2]}
               1))))

(defn part2 [player]
  (run-game2 player (input)))
