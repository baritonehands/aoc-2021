(ns aoc.dec3
  (:require [aoc.utils :as utils]))

(defn input []
  (->> (utils/day-file 3)
       (mapv vec)))

(defn bit-freq [xs]
  (->> xs
       (mapcat #(mapv vector (range) %))
       (frequencies)
       (into (sorted-map))))

(defn seq->bin [xs]
  (Integer/parseInt (apply str xs) 2))

(defn frequent [op freq]
  (for [i (range 0 (/ (count freq) 2))
        :let [zero (get freq [i \0])
              one (get freq [i \1])]]
    (if (op zero one)
      0
      1)))

(defn part1 []
  (let [data (input)
        freq (bit-freq data)]
    (* (seq->bin (frequent > freq))
       (seq->bin (frequent < freq)))))

(defn cascade [op xs]
  (let [size (count (first xs))]
    (loop [n 0
           remaining xs]
      (if (or (>= n size)
              (= 1 (count remaining)))
        (seq->bin (first remaining))
        (let [{zero \0
               one  \1} (frequencies (map #(get % n) remaining))
              to-keep (cond
                        (nil? zero) \1
                        (nil? one) \0
                        (op zero one) \0
                        :else \1)]
          (recur
            (inc n)
            (filter
              #(= (get % n) to-keep)
              remaining)))))))

(defn part2 []
  (let [data (input)]
    (* (cascade > data) (cascade <= data))))

