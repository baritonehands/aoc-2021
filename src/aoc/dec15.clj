(ns aoc.dec15
  (:require [aoc.utils :as utils]
            [clojure.string :as str])
  (:import (java.util PriorityQueue Comparator HashMap)
           (java.util.function ToLongFunction)))

(defn parse-line [line]
  (mapv #(- (int %) (int \0)) line))

(defn input []
  (->> (utils/day-file 15)
       (mapv parse-line)))

(defn ny-distance [[x y] [cx cy]]
  (+ (Math/abs ^long (- cy y))
     (Math/abs ^long (- cx x))))

(defn neighbors [[x y]]
  (set
    (for [[dx dy] [[1 0] [0 1] [-1 0] [0 -1]]]
      [(+ x dx) (+ y dy)])))

(defn walk-path [came-from current]
  (loop [current current
         path (list current)]
    (if (contains? came-from current)
      (recur (came-from current) (conj path (came-from current)))
      path)))

(defn priority-queue [f-score & elems]
  (doto
    (PriorityQueue.
      (Comparator/comparingLong
        (reify ToLongFunction
          (applyAsLong [_ e]
            (get f-score e Long/MAX_VALUE)))))
    (.addAll elems)))

(defn safest-path [risks start end]
  (let [weight (fn [pos] (get-in risks (reverse pos)))
        f-score (HashMap. {start (ny-distance start end)})
        open-set (priority-queue f-score start)]
    (loop [[came-from g-score]
           [{} {start 0}]]
      (if (.isEmpty open-set)
        :error
        (let [current (.peek open-set)]
          (if (= current end)
            (walk-path came-from current)
            (do
              (.remove open-set)
              (recur
                (loop [[neighbor & more] (->> (neighbors current)
                                              (filter
                                                (fn [[nx ny]]
                                                  (and (>= nx 0) (>= ny 0)
                                                       (<= nx (first end))
                                                       (<= ny (second end))))))
                       cf-inner came-from
                       gs-inner g-score]
                  (if-not neighbor
                    [cf-inner gs-inner]
                    (let [g (+ (gs-inner current) (weight neighbor))]
                      (if (or (not (gs-inner neighbor))
                              (< g (gs-inner neighbor)))
                        (do
                          (.put f-score neighbor (+ g (ny-distance neighbor end)))
                          (.add open-set neighbor)
                          (recur
                            more
                            (assoc cf-inner neighbor current)
                            (assoc gs-inner neighbor g)))
                        (recur more cf-inner gs-inner)))))))))))))

(defn part1 []
  (let [data (input)
        xmax (dec (count (first data)))
        ymax (dec (count data))]
    (->> (safest-path data [0 0] [xmax ymax])
         (drop 1)
         (map #(get-in data (reverse %)))
         (reduce +))))

(defn add-wrap [a b]
  (let [sum (+ a b)]
    (if (> sum 9)
      (- sum 9)
      sum)))

(defn expand-y [xdata]
  (vec (for [my (range 0 5)
             row xdata]
         (vec (for [col row]
                (add-wrap col my))))))

(defn expand-x [data]
  (vec (for [row data]
         (vec (for [mx (range 0 5)
                    col row]
                (add-wrap col mx))))))

(defn part2 []
  (let [data (-> (input)
                 (expand-x)
                 (expand-y))
        xmax (dec (count (first data)))
        ymax (dec (count data))]
    (->> (safest-path data [0 0] [xmax ymax])
         (drop 1)
         (map #(get-in data (reverse %)))
         (reduce +))))
