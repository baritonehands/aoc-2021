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

(defn v+ [[lx ly lz] [rx ry rz]]
  [(+ lx rx) (+ ly ry) (+ lz rz)])

(def z-rotations
  [(juxt z x y)
   (juxt z y (comp - x))
   (juxt z (comp - x) (comp - y))
   (juxt z (comp - y) x)
   (juxt (comp - z) (comp - x) y)
   (juxt (comp - z) y x)
   (juxt (comp - z) x (comp - y))
   (juxt (comp - z) (comp - y) (comp - x))])

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

(defn resolve-scanners [data]
  (loop [resolved [[(first data) [0 0 0]]]
         unresolved (set (drop 1 data))]
    (println (count resolved) (count unresolved))
    (if (empty? unresolved)
      resolved
      (let [to-resolve (for [[lhs lpos] resolved
                             rhs unresolved
                             :let [overlap (find-overlap lhs rhs)]
                             :when overlap]
                         (let [[rpos [axis orientation]] overlap
                               rotate (get (get axes axis) orientation)]
                           [(mapv rotate rhs)
                            (v+ lpos rpos)
                            rhs]))
            pairs (map (juxt first second) to-resolve)
            to-remove (set (map peek to-resolve))]
        (if (empty? pairs)
          resolved
          (recur (into resolved pairs) (set/difference unresolved to-remove)))))))

(defn part1 []
  (let [data (input)
        resolved (resolve-scanners data)]
    (->> (for [[scanner pos] resolved
               coord scanner]
           (v+ coord pos))
         (distinct)
         (count))))
