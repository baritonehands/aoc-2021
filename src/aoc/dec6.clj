(ns aoc.dec6
  (:require [aoc.utils :as utils]
            [clojure.string :as str])
  (:import (java.util ArrayList)))

(defn input []
  (ArrayList.
    (mapv #(Integer/parseInt %)
          (-> (utils/day-file 6)
              (first)
              (str/split #",")))))

(defn generation [cur]
  (let [size (.size cur)
        to-append (ArrayList.)]
    (loop [n 0]
      (if (= n size)
        (do
          (.addAll cur to-append)
          cur)
        (let [fish (.get cur n)]
          (if (zero? fish)
            (do
              (.set cur n 6)
              (.add to-append 8))
            (.set cur n (dec fish)))
          (recur (inc n)))))))

(defn part1 []
  (let [data (input)]
    (loop [cur data
           n 0]
      (if (= n 80)
        (count cur)
        (do
          (generation cur)
          (recur cur (inc n)))))))

(defn part2 []
  (let [data (input)]
    (loop [cur data
           n 0]
      (println n)
      (if (= n 256)
        (count cur)
        (do
          (generation cur)
          (recur cur (inc n)))))))
