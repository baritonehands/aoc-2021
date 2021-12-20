(ns aoc.dec20
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def x first)
(def y second)

(defn input []
  (let [[enhancement image] (->> (utils/day-file 20)
                                 (split-with #(not= % "")))]
    [(->> enhancement
          (reduce str)
          (mapv #(= % \#)))
     (set (for [[r-idx row] (map-indexed vector (drop 1 image))
                [c-idx col] (map-indexed vector row)
                :when (= col \#)]
            [c-idx r-idx]))]))

(defn bounds [image]
  (let [xmin (utils/min-by x image)
        xmax (utils/max-by x image)
        ymin (utils/min-by y image)
        ymax (utils/max-by y image)]
    [[xmin (inc xmax)] [ymin (inc ymax)]]))

(defn set->pixel [s]
  (fn [[x y]]
    (if (get s [x y]) \1 \0)))

(defn pixel-source [image [x y]]
  (let [above [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]
        mid [[(dec x) y] [x y] [(inc x) y]]
        below [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
        lookup (set->pixel image)
        bin (->> (concat
                   (map lookup above)
                   (map lookup mid)
                   (map lookup below))
                 (reduce str))]
    (Long/parseLong bin 2)))

(defn fix-negative [image]
  (let [[[xmin xmax] [ymin ymax]] (bounds image)
        dx (- xmin)
        dy (- ymin)]
    (println dx dy)
    (for [[x y] image]
      [(+ x dx) (+ y dy)])))

(defn enhance [enhancement image]
  (let [[[xmin xmax] [ymin ymax]] (bounds image)]
    (->> (for [y (range (dec ymin) (inc ymax))
               x (range (dec xmin) (inc xmax))
               :let [pixel (pixel-source image [x y])]
               :when (get enhancement pixel)]
           [x y])
         ;fix-negative
         set)))

(defn print-image
  ([image] (print-image image (bounds image)))
  ([image bounds]
   (let [[[xmin xmax] [ymin ymax]] bounds]
     (->> (for [y (range ymin ymax)]
            (->> (for [x (range xmin xmax)
                       :let [hash? (get image [x y])]]
                   (if hash? \# \.))
                 (reduce str)))
          (str/join "\n")
          (println)))))

(defn part1 []
  (let [[enhancement image] (input)
        [[xmin xmax] [ymin ymax]] (bounds image)]
    (-> image
        (doto (print-image))
        (->> (enhance enhancement))
        (doto (print-image))
        (->> (enhance enhancement))
        (doto print-image)
        (count))))

