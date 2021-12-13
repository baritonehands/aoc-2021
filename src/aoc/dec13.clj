(ns aoc.dec13
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse-line [line]
  (cond
    (= line "")
    nil

    (.startsWith line "fold")
    (str/split (.substring line 11) #"=")

    :else
    (->> (str/split line #",")
         (mapv #(Integer/parseInt %)))))

(defn input []
  (let [all (->> (utils/day-file 13)
                 (mapv parse-line))]
    [(->> all
          (take-while some?)
          (set))
     (->> all
          (drop-while some?)
          (rest)
          (vec))]))

(defn fold [dots [dir idx]]
  (let [xmax (inc (utils/max-by first dots))
        ymax (inc (utils/max-by second dots))
        idx (Integer/parseInt idx)]
    (set
      (for [x (range 0 xmax)
            y (range 0 ymax)
            :let [orig (get dots [x y])]
            :when orig]
        (cond
          (and (= dir "y") (> y idx))
          [x (+ idx (- idx y))]

          (and (= dir "x") (> x idx))
          [(+ idx (- idx x)) y]

          :else [x y])))))

(defn part1 []
  (let [[dots inst] (input)]
    (count
      (reduce
        fold
        dots
        (take 1 inst)))))

(defn print-dots [dots]
  (let [xmax (inc (utils/max-by first dots))
        ymax (inc (utils/max-by second dots))]
    (println
      (for [y (range 0 ymax)]
        (->> (for [x (range 0 xmax)]
               (if (get dots [x y])
                 "X"
                 " "))
             (concat ["\n"])
             (apply str))))))

(defn part2 []
  (let [[dots inst] (input)
        result (reduce
                 fold
                 dots
                 inst)]
    (print-dots result)))
