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
                    (find-paths {:level   (inc level)
                                 :path    (conj path 0)
                                 :results []} l)
                    [[(conj path 0) l]])
           next-r (if (vector? r)
                    (find-paths {:level   (inc level)
                                 :path    (conj path 1)
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

(defn find-number2
  ([xs target-path] (find-number2 xs target-path))
  ([{:keys [level path]} [l r :as xs] target-path]
   (if (= level 4)
     [l r]
     (let [[node & more] (tree-seq sequential? seq xs)]))))

(defn tree-seq-depth
  [branch? children root]
  (let [walk (fn walk [depth path node]
               (lazy-seq
                 (cons {:depth depth :path path :node node}
                       (when (branch? node)
                         (mapcat (fn [[idx child]]
                                   (walk (inc depth) (conj path idx) child)) (map-indexed vector (children node)))))))]
    (walk 0 [] root)))

(defn number-path [xs]
  (->> xs
       (filter #(number? (:node %)))
       first
       :path))

(defn explode [xs]
  (let [[lhs rhs] (->> (tree-seq-depth sequential? seq xs)
                       (split-with (fn [{:keys [depth node]}]
                                     (or (not= depth 4) (number? node)))))]
    (if (first rhs)
      (let [{path  :path
             [l r] :node} (-> rhs first)
            l-path (->> lhs
                        reverse
                        number-path)
            r-path (->> (rest rhs)
                        (drop-while #(> (:depth %) 4))
                        number-path)]
           (cond-> xs
                   path (assoc-in path 0)
                   (some? l-path) (update-in l-path #(+ % l))
                   (some? r-path) (update-in r-path #(+ % r))))
      xs)))

(defn split [xs]
  (if-let [{:keys [path node]} (->> (tree-seq-depth sequential? seq xs)
                                    (filter #(and (number? (:node %))
                                                  (> (:node %) 9)))
                                    (first))]
    (-> xs
        (assoc-in path (let [l (long (Math/floor (/ node 2)))
                             r (long (Math/ceil (/ node 2)))]
                         [l r])))
    xs))


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
  (loop [result [l r]]
    (let [exploded (explode result)]
      (if (not= exploded result)
        (recur exploded)
        (let [twain (split result)]
          (if (not= twain result)
            (recur twain)
            result))))))

(defn magnitude [xs]
  (let [[l r] xs]
    (+ (* 3 (cond-> l (vector? l) magnitude))
       (* 2 (cond-> r (vector? r) magnitude)))))

(defn part1 [& data]
  (magnitude
    (reduce add data)))

(defn part2 [& data]
  (->> (for [l data
             r data
             :when (not= l r)]
         (magnitude (add l r)))
       (sort)))

(comment
  (= (explode [[[[[9, 8], 1], 2], 3], 4])
     [[[[0, 9], 2], 3], 4])
  (= (explode [7, [6, [5, [4, [3, 2]]]]])
     [7, [6, [5, [7, 0]]]])
  (= (explode [[6, [5, [4, [3, 2]]]], 1])
     [[6, [5, [7, 0]]], 3])
  (= (explode [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]])
     [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]))
