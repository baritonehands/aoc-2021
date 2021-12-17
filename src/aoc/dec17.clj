(ns aoc.dec17
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn input []
  (let [[[sx ex] [sy ey]] (-> (first (utils/day-file 17))
                              (str/split #",* ")
                              (->> (drop 2)
                                   (map (comp #(str/split % #"\.\.") #(.substring % 2)))))]
    [[(Long/parseLong sx) (Long/parseLong ex)]
     [(Long/parseLong sy) (Long/parseLong ey)]]))

(defn passed? [[[sx ex] [sy ey]] [x y]]
  (or (> x ex)
      (< y sy)))

(defn in-target? [[[sx ex] [sy ey]] [x y]]
  (and (>= x sx) (<= x ex)
       (>= y sy) (<= y ey)))

(defn compute [target [vx vy]]
  (loop [[x y] [0 0]
         ymax 0
         [vx vy] [vx vy]]
    (cond
      (in-target? target [x y]) ymax
      (passed? target [x y]) false
      :else (recur
              [(+ x vx) (+ y vy)]
              (max ymax (+ y vy))
              [(cond
                 (pos? vx) (dec vx)
                 (neg? vx) (inc vx)
                 :else 0)
               (dec vy)]))))

(defn part1 []
  (let [target (input)]
    (->> (for [vx (range 0 200)
               vy (range -106 1000)
               :let [ymax (compute target [vx vy])]
               :when (int? ymax)]
           [[vx vy] (compute target [vx vy])])
         (sort-by second))))

