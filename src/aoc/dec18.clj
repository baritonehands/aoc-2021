(ns aoc.dec18
  (:require [aoc.utils :as utils]
            [clojure.walk :as walk]))

(defn input []
  (utils/day-file 18))

(defn find-explosion
  ([xs] (find-explosion {:level 0
                         :path  []} xs))
  ([{:keys [level path]} [l r :as pair]]
   (if (= level 4)
     path
     (if-let [l-result (and (vector? l)
                            (find-explosion {:level (inc level)
                                             :path  (conj path 0)} l))]
       l-result
       (and (vector? r)
            (find-explosion {:level (inc level)
                             :path  (conj path 1)} r))))))

(defn find-paths
  ([xs] (find-paths {:level   0
                     :path    []
                     :results []} xs))
  ([{:keys [level path results] :as state} [l r :as pair]]
   (if (= level 4)
     (into results [[(conj path 0) l] [(conj path 1) r]])
     (let [next-l (if (vector? l)
                    (find-paths {:level (inc level)
                                 :path  (conj path 0)
                                 :results []} l)
                    [[(conj path 0) l]])
           next-r (if (vector? r)
                    (find-paths {:level (inc level)
                                 :path  (conj path 1)
                                 :results []} r)
                    [[(conj path 1) l]])]
       (into results (concat next-l next-r))))))

(defn find-number [xs path dir]
  (loop [path (pop path)]
    (println "find-number" path dir)
    (let [possible (get-in xs path)]
      (println "possible" possible)
      (cond
        (number? (get possible dir)) (conj path dir)
        :else (cond
                (nil? (peek path))
                nil

                (= (peek path) dir)
                (recur (pop path))

                :else
                (let [next-path (update path (dec (count path)) #(if (= % 0) 1 0))]
                  (println "dir change" next-path)
                  (recur next-path)))))))

(defn explode [xs path]
  (let [[l r] (get-in xs path)
        l-path (find-number xs path 0)
        r-path (find-number xs path 1)]
    (println l-path r-path)
    (cond-> xs
            true (assoc-in path 0)
            (some? l-path) (update-in l-path #(+ % l))
            (some? r-path) (update-in r-path #(+ % r)))))

(defn reduce+
  ([xs] (reduce+ {:level 0
                  :path  []} xs))
  ([{:keys [level path explode split] :as state} [l r]]
   (if (or explode split)
     [l r]
     (let [next-l (if (vector? l)
                    (reduce+ {:level (inc level)
                              :path  (conj path 0)} l)
                    l)
           next-r (if (vector? r)
                    (reduce+ {:level (inc level)
                              :path  (conj path 1)} r)
                    r)]
       (println state [next-l next-r])
       [next-l next-r]))))


(defn add [l r]
  (-> [l r]
      (reduce+)))

(defn part1 []
  (let [data (input)]
    data))

(comment
  (= (explode [[[[[9, 8], 1], 2], 3], 4] [0 0 0 0])
     [[[[0, 9], 2], 3], 4])
  (= (explode [7, [6, [5, [4, [3, 2]]]]] [1 1 1 1])
     [7, [6, [5, [7, 0]]]])
  (= (explode [[6, [5, [4, [3, 2]]]], 1] [0 1 1 1])
     [[6, [5, [7, 0]]], 3])
  (= (explode [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]] [0 1 1 1])
     [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]))
