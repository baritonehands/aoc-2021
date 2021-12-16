(ns aoc.dec16
  (:require [aoc.utils :as utils]))

(def hex->bin
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn input []
  (->> (utils/day-file 16)
       (first)
       (map hex->bin)
       (reduce str)))

(defn literal? [type]
  (= type [\1 \0 \0]))

(declare parse)

(defn parse-literal [s]
  (let [[literal-str rem] (loop [[digit & [rem]] (split-at 5 s)
                                 result []]

                            (if (= (first digit) \0)
                              [(->> (drop 1 digit)
                                    (into result)
                                    (apply str))
                               rem]
                              (recur (split-at 5 rem) (into result (drop 1 digit)))))]
    [(Long/parseLong literal-str 2) rem]))

(defn parse-subpackets [[len-type & rem]]
  (case len-type
    \0 (let [[len-str & [packets]] (split-at 15 rem)
             len (Long/parseLong (apply str len-str) 2)]
         [(-> (take len packets) parse first) (drop len packets)])
    \1 (let [[cnt-str & [packets]] (split-at 11 rem)
             cnt (Long/parseLong (apply str cnt-str) 2)]
         (parse cnt packets))))

(defn next-packet [packets]
  (let [ver (take 3 packets)
        type (->> packets (drop 3) (take 3))
        rem (drop 6 packets)
        packet {:ver  (Long/parseLong (apply str ver) 2)
                :type (Long/parseLong (apply str type) 2)}
        [value unparsed] (if (literal? type)
                           (parse-literal rem)
                           (parse-subpackets rem))]
    [(assoc packet :value value) unparsed]))

(defn parse
  ([s] (parse -1 s))
  ([n s]
   (loop [cnt 1
          [packet & [rem]] (next-packet s)
          results []]
     (if (or (<= (count rem) 6) (= cnt n))
       [(conj results packet) (seq rem)]
       (recur (inc cnt) (next-packet rem) (conj results packet))))))

(defn sum-versions [packets]
  (reduce
    (fn [total {:keys [ver value]}]
      (if (vector? value)
        (+ total ver (sum-versions value))
        (+ total ver)))
    0
    packets))

(defn part1 []
  (let [data (input)
        [packets] (parse data)]
    (sum-versions packets)))

(defn compute [{:keys [type value]}]
  (case type
    4 value
    0 (reduce + (map compute value))
    1 (reduce * (map compute value))
    2 (reduce min (map compute value))
    3 (reduce max (map compute value))
    5 (let [[l r] (map compute value)]
        (if (> l r) 1 0))
    6 (let [[l r] (map compute value)]
        (if (< l r) 1 0))
    7 (let [[l r] (map compute value)]
        (if (= l r) 1 0))))

(defn part2 []
  (let [data (input)
        [packets] (parse data)]
    (compute (first packets))))
