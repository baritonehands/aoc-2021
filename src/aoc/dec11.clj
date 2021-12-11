(ns aoc.dec11
  (:require [aoc.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn input []
  (->> (utils/day-file 11)
       (mapv (partial mapv #(- (int %) (int \0))))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [nx (+ x dx)
              ny (+ y dy)]
        :when (and (or (not= dx 0) (not= dy 0))
                   (>= nx 0)
                   (>= ny 0)
                   (< nx 10)
                   (< ny 10))]
    [nx ny]))

(defn flashing [state]
  (set
    (for [x (range 0 10)
          y (range 0 10)
          :when (> (get-in state [y x]) 9)]
      [x y])))

(defn reset-flash [state]
  (vec (for [row state]
         (vec (for [col row]
                (if (> col 9)
                  0
                  col))))))

(defn flash [{:keys [state cnt]}]
  (loop [state (vec (for [row state]
                      (vec (for [col row]
                             (inc col)))))
         flashed #{}
         to-flash (-> (flashing state)
                      (set/difference flashed))]
    (if (empty? to-flash)
      (let [result (reset-flash state)]
        {:state result
         :cnt   (+ cnt (->> result (mapcat identity) (filter zero?) count))})
      (let
        [updated (reduce
                   (fn [next-state pos]
                     (update-in next-state (reverse pos) inc))
                   state
                   (mapcat neighbors to-flash))
         next-flashed (into flashed to-flash)]
        (recur
          updated
          next-flashed
          (set/difference (flashing updated) next-flashed))))))


(defn part1
  ([n] (part1 n (input)))
  ([n data]
   (->> {:state data
         :cnt   0}
        (iterate flash)
        (drop n)
        (first))))

(defn part2 []
  (let [data (input)]
    (loop [state {:state data
                  :cnt 0}
           step 1]
      (let [next-state (flash state)]
        (if (->> next-state :state flatten (every? zero?))
          step
          (recur next-state (inc step)))))))

