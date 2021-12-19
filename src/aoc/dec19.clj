(ns aoc.dec19
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn input []
  (vec (for [scanner (->> (utils/day-file 19)
                          (remove #(or (.startsWith % "---")))
                          (partition-by #(= % ""))
                          (remove #(= (count %) 1)))]
         (vec (for [row scanner]
                (vec (for [coord (str/split row #",")]
                       (Long/parseLong coord))))))))

(def x first)
(def y second)
(def z (fn [[_ _ z]] z))

(defn v- [[lx ly lz] [rx ry rz]]
  [(- lx rx) (- ly ry) (- lz rz)])

(def z-rotations
  [(juxt z x y)
   (juxt z y (comp - x))
   (juxt z (comp - x) (comp - y))
   (juxt z (comp - y) x)
   (juxt (comp - z) (comp - x) y)
   (juxt (comp - z) y x)
   (juxt (comp - z) x (comp - y))
   (juxt (comp - z) (comp - x) (comp - y))])

(def y-rotations
  [(juxt y x (comp - z))
   (juxt y (comp - z) (comp - x))
   (juxt y (comp - x) z)
   (juxt y z x)
   (juxt (comp - y) x z)
   (juxt (comp - y) z (comp - x))
   (juxt (comp - y) (comp - x) (comp - z))
   (juxt (comp - y) (comp - z) x)])

(def x-rotations
  [(juxt x (comp - z) y)
   (juxt x y z)
   (juxt x z (comp - y))
   (juxt x (comp - y) (comp - z))
   (juxt (comp - x) z y)
   (juxt (comp - x) y (comp - z))
   (juxt (comp - x) (comp - z) (comp - y))
   (juxt (comp - x) (comp - y) z)])

(def axes [x-rotations y-rotations z-rotations])

(def orientations
  (into {}
        (for [idx (range 0 3)
              ridx (range 0 8)
              :let [axis (get axes idx)
                    rotation (get axis ridx)]]
          [[idx ridx] rotation])))

(defn all-combos [scanner]
  (->> (for [v scanner
             [k f] orientations]
         [(f v) k])
       (into {})))

(defn find-overlap [lhs rhs]
  (->> (for [l-beacon lhs
             [r-beacon k] (all-combos rhs)]
         [(v- l-beacon r-beacon) k])
       (frequencies)
       (filter #(>= (second %) 12))
       (ffirst)))

(defn unique-beacons [data overlaps]
  (println (first overlaps))
  (->> (for [[lr [pos orientation]] overlaps
             :let [[lhs rhs] (map data lr)
                   tx (get orientations orientation)]]
         (for [l-beacon lhs
               r-beacon (map tx rhs)
               :when (= r-beacon (v- l-beacon pos))]
           l-beacon))
       (mapcat identity)
       (distinct)))

(defn overlap->path [overlaps]
  (let [all (->> (keys overlaps)
                 (mapcat identity)
                 (set))]
    all))


(defn part1 []
  (let [data (input)
        overlaps (->> (for [[l-idx lhs] (map-indexed vector data)
                            [r-idx rhs] (map-indexed vector data)
                            :when (not= r-idx l-idx)
                            :let [overlap (find-overlap lhs rhs)]]
                        [[l-idx r-idx] overlap])
                      (remove (comp nil? second second))
                      (into {}))]
    overlaps))
    ;(unique-beacons data overlaps)))



