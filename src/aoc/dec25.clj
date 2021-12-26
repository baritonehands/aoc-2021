(ns aoc.dec25
  (:require [aoc.utils :as utils]))

(defn input []
  (->> (utils/day-file 25)
       (mapv vec)))

(defn bounds [map]
  (let [xmax (dec (count (first map)))
        ymax (dec (count map))]
    [xmax ymax]))

(defn lookup [map [x y]]
  (let [[xmax ymax] (bounds map)
        px (cond
             (> x xmax) 0
             (= x -1) xmax
             :else x)
        py (cond
             (> y ymax) 0
             (= y -1) ymax
             :else y)]
    (get-in map [py px])))

(defn move [map dir]
  (let [+pos (fn [[x y] n]
               (if (= dir \>)
                 [(+ x n) y]
                 [x (+ y n)]))]
    (vec (for [[y row] (map-indexed vector map)]
           (vec (for [[x col] (map-indexed vector row)]
                  (cond
                    (and (= col dir)
                         (= (lookup map (+pos [x y] 1)) \.))
                    \.

                    (and (= col \.)
                         (= (lookup map (+pos [x y] -1)) dir))
                    dir

                    :else col)))))))

(defn step [map]
  (-> map
      (move \>)
      (move \v)))

(defn part1 []
  (let [data (input)]
    (loop [prev data
           cur (step data)
           cnt 1]
      (if (= prev cur)
        cnt
        (recur cur (step cur) (inc cnt))))))
