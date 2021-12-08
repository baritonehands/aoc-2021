(ns aoc.dec8
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[signal output] (str/split line #" \| ")]
    [(str/split signal #" ")
     (str/split output #" ")]))

(defn input []
  (for [[signal output] (map parse-line (utils/day-file 8))]
    [signal output]))

(defn one? [digit]
  (= (.length digit) 2))

(defn four? [digit]
  (= (.length digit) 4))

(defn seven? [digit]
  (= (.length digit) 3))

(defn eight? [digit]
  (= (.length digit) 7))

(def uniques (juxt one? four? seven? eight?))

(defn part1 []
  (let [data (input)
        unique-count #(count (filter true? (mapcat uniques %)))]
    (->> (for [[_ output] data]
           (unique-count output))
         (reduce +))))

(defn find-set [f xs]
  (set (first (filter f xs))))

; Copied from https://www.reddit.com/r/adventofcode/comments/rbj87a/comment/hnorgs4/?utm_source=share&utm_medium=web2x&context=3
(defn signal-key [signal]
  (let [counts (frequencies (mapcat identity signal))
        one (find-set one? signal)
        four (find-set four? signal)
        seven (find-set seven? signal)
        eight (find-set eight? signal)
        six (->> (filter #(= (.length %) 6) signal)
                 (filter #(seq (set/difference one (set %))))
                 (first)
                 (set))
        a (first (set/difference seven one))
        b (ffirst (filter #(= (val %) 6) counts))
        c (first (set/difference one six))
        e (ffirst (filter #(= (val %) 4) counts))
        f (ffirst (filter #(= (val %) 9) counts))
        g (first (set/difference eight #{a b c e f} four))
        d (ffirst (dissoc counts a b c e f g))
        two #{a, c, d, e, g}
        three #{a, c, d, f, g}
        five #{a, b, d, f, g}
        nine #{a, b, c, d, f, g}
        zero #{a, b, c, e, f, g}]
    {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}))

(defn part2 []
  (let [data (input)]
    (->> (for [[signal output] data
               :let [k (signal-key signal)]]
           (->> (map set output)
                (map k)
                (apply str)))
         (map #(Integer/parseInt %))
         (reduce +))))
