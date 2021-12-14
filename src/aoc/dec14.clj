(ns aoc.dec14
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn split-pair [pair]
  (let [[pair ch] (str/split pair #" -> ")]
    [(seq pair) (first ch)]))

(defn input []
  (let [[template _ & pairs] (utils/day-file 14)]
    [template (->> pairs
                   (map split-pair)
                   (into {}))]))

(defn window [pairs]
  (->> (partition-all 2 1 pairs)
       (butlast)))

(defn butlastv [v]
  (if (zero? (count v))
    v
    (subvec v 0 (dec (count v)))))

(defn react [rules result [l r :as pair]]
  (if-let [rule (get rules pair)]
    (into (butlastv result) [l rule r])
    (into result [r])))

(defn step [pairs template]
  (reduce
    #(react pairs %1 %2)
    []
    (window template)))

(defn part1
  ([] (part1 10))
  ([n]
   (let [[template pairs] (input)]
     (->> (seq template)
          (iterate (partial step pairs))
          (drop n)
          (first)
          (frequencies)
          (sort-by val)))))

(defn react2 [rules [pairs elements] [[l r :as pair] n]]
  (if-let [m (get rules pair)]
    [(-> pairs
         (update pair - n)
         (update [l m] (fnil + 0) n)
         (update [m r] (fnil + 0) n))
     (update elements m (fnil + 0) n)]
    [pairs elements]))


(defn step2 [pairs [pfreq efreq]]
  (reduce
    (partial react2 pairs)
    [pfreq efreq]
    pfreq))

(defn part2
  ([] (part2 10))
  ([n]
   (let [[template pairs] (input)]
     (->> [(frequencies (window template))
           (frequencies template)]
          (iterate (partial step2 pairs))
          (drop n)
          (first)
          (second)
          (sort-by val)))))
