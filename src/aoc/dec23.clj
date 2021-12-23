(ns aoc.dec23
  (:require [aoc.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.util Comparator PriorityQueue HashMap)
           (java.util.function ToLongFunction)))

(def space? #{\.})
(def pod? #{\A \B \C \D})
(def spot? (set/union space? pod?))

(defn input []
  (let [lines (utils/day-file 23)
        ymax (count lines)
        xmax (count (first lines))
        spots (for [y (range 0 (inc ymax))
                    x (range 0 (inc xmax))
                    :let [spot (get-in lines [y x])]
                    :when (spot? spot)]
                [[x y] spot])]
    [{:spaces (->> spots (filter #(space? (second %))) (map first) set)
      :pods   (->> spots (filter #(pod? (second %))) (into {}))}
     (->> spots (map first) set)]))

(def cost
  {\A 1
   \B 10
   \C 100
   \D 1000})

(def targets
  {\A #{[3 3] [3 2]}
   \B #{[5 3] [5 2]}
   \C #{[7 3] [7 2]}
   \D #{[9 3] [9 2]}})

(defn target-room? [pod [x y]]
  (-> (targets pod) (contains? [x y])))

(def room?
  (->> targets
       vals
       (reduce set/union)))
(def hallway? (complement room?))

(defn valid-room? [{:keys [spaces pods]} pod [x y]]
  (let [target (targets pod)
        sibling? (every? #(or (nil? %) (= % pod)) (map pods target))]
    (and (contains? target [x y])
         (or sibling?
             (= 2 (set/intersection spaces target))))))

(defn ny-distance [[x y] [cx cy]]
  (+ (Math/abs ^long (- cy y))
     (Math/abs ^long (- cx x))))

(def neighbors
  {[1 1]  #{[2 1]}
   [2 1]  #{[1 1] [3 1]}
   [3 1]  #{[2 1] [4 1] [3 2]}
   [3 2]  #{[3 1] [3 3]}
   [3 3]  #{[3 2]}
   [4 1]  #{[3 1] [5 1]}
   [5 1]  #{[4 1] [6 1] [5 2]}
   [5 2]  #{[5 1] [5 3]}
   [5 3]  #{[5 2]}
   [6 1]  #{[5 1] [7 1]}
   [7 1]  #{[6 1] [8 1] [7 2]}
   [7 2]  #{[7 1] [7 3]}
   [7 3]  #{[7 2]}
   [8 1]  #{[7 1] [9 1]}
   [9 1]  #{[8 1] [10 1] [9 2]}
   [9 2]  #{[9 1] [9 3]}
   [9 3]  #{[9 2]}
   [10 1] #{[9 1] [11 1]}
   [11 1] #{[10 1]}})

(defn walk-path [came-from current]
  (loop [current current
         path (list current)]
    (if (contains? came-from current)
      (recur (came-from current) (conj path (came-from current)))
      path)))

(defn priority-queue [f-score & elems]
  (doto
    (PriorityQueue.
      (Comparator/comparingLong
        (reify ToLongFunction
          (applyAsLong [_ e]
            (get f-score e Long/MAX_VALUE)))))
    (.addAll elems)))

(defn shortest-path [start end]
  (let [f-score (HashMap. {start (ny-distance start end)})
        open-set (priority-queue f-score start)]
    (loop [[came-from g-score]
           [{} {start 0}]]
      (if (.isEmpty open-set)
        :error
        (let [current (.peek open-set)]
          (if (= current end)
            (walk-path came-from current)
            (do
              (.remove open-set)
              (recur
                (loop [[neighbor & more] (neighbors current)
                       cf-inner came-from
                       gs-inner g-score]
                  (if-not neighbor
                    [cf-inner gs-inner]
                    (let [g (+ (gs-inner current) 1)]
                      (if (or (not (gs-inner neighbor))
                              (< g (gs-inner neighbor)))
                        (do
                          (.put f-score neighbor (+ g (ny-distance neighbor end)))
                          (.add open-set neighbor)
                          (recur
                            more
                            (assoc cf-inner neighbor current)
                            (assoc gs-inner neighbor g)))
                        (recur more cf-inner gs-inner)))))))))))))

(defn generate-paths [all]
  (into {} (for [from all
                 to all
                 :when (not= from to)]
             [[from to] (drop 1 (shortest-path from to))])))

(def outside-rooms?
  #{[3 1] [5 1] [7 1] [9 1]})

(defn valid-destination? [state [[x y] pod] [ex ey]]
  (if (or (outside-rooms? [ex ey])
          (and ;(hallway? [x y])
               (room? [ex ey])
               (not (valid-room? state pod [ex ey])))
          (and (hallway? [x y])
               (hallway? [ex ey]))
          (and (target-room? pod [x y])
               (or (= y 3)
                   (= pod (get-in state [:pods [x 3]])))))

    false
    true))

(defn complete? [{:keys [pods]}]
  (= targets (reduce
               (fn [m [from pod]]
                 (update m pod (fnil conj #{}) from))
               {}
               pods)))

(defn path-clear? [spaces path]
  (every? spaces path))

(defn print-state [{:keys [spaces pods] :as state}]
  (println state)
  (->> (for [y (range 0 5)]
         (->> (for [x (range 0 13)
                    :let [pod (get pods [x y])]]
                (cond
                  pod pod
                  (get spaces [x y]) \.
                  (or (#{0 4} y)
                      (= (mod x 2) 0)) \#
                  :else " "))
              (reduce str)))
       (str/join "\n")
       (println))
  (println "\n\n"))

(defn part1 [n]
  (let [[initial-state all] (input)
        paths (generate-paths all)
        oop (for [[[x y] pod] (:pods initial-state)
                  :when (-> (targets pod)
                            (contains? [x y])
                            (not))]
              [[x y] pod])
        destinations (fn [{:keys [pods spaces] :as state}]
                       (for [[from pod] pods
                             space spaces
                             :let [path (get paths [from space])]
                             :when (and (valid-destination? state [from pod] space)
                                        (path-clear? spaces path))]
                         [state {:pod pod
                                 :from from
                                 :to space
                                 :energy (* (cost pod) (count path))}]))
        move (fn [state {:keys [pod from to]}]
               (-> state
                   (update :spaces #(-> % (disj to) (conj from)))
                   (update :pods #(-> % (dissoc from) (assoc to pod)))))]
    (loop [cnt 0
           cur-min (Long/MAX_VALUE)
           states {}
           [[state dest] & more] (destinations initial-state)
           complete #{}]
      (if (= (mod cnt 10000) 0)
        (println cnt (count states) (count more) cur-min))
      (if (nil? state)
        (map states complete)
        (let [next-state (move state dest)]
          ;(print-state next-state)
          (cond
            ;(= cnt n)
            ;states

            (complete? next-state)
            (let [next-energy (+ (:energy dest)
                                 (get states state 0))
                  next-min (min cur-min next-energy)]
              (recur (inc cnt)
                     next-min
                     (update states next-state (fnil min Long/MAX_VALUE) (+ (:energy dest)
                                                                            (get states state 0)))
                     (remove (fn [[state {:keys [energy]}]]
                               (> (+ (get states state 0) energy) next-min)) more)
                     (conj complete next-state)))

            :else
            (let [next-energy (+ (:energy dest)
                                 (get states state 0))]
              (if (< next-energy (get states next-state Long/MAX_VALUE))
                (recur (inc cnt)
                       cur-min
                       (update states next-state (fnil min Long/MAX_VALUE) next-energy)
                       (->> (concat (destinations next-state) more)
                            (remove (fn [[state {:keys [energy]}]]
                                      (> (+ (get states state 0) energy) cur-min))))
                       complete)
                (recur (inc cnt)
                       cur-min
                       states
                       more
                       complete)))))))))
