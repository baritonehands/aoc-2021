(ns aoc.dec9
  (:require [aoc.utils :as utils]))

(defn char->int [ch]
  (- (int ch) (int \0)))

(defn input []
  (->> (utils/day-file 9)
       (map #(map char->int %))
       (mapv vec)))

(defn lowest? [v adj]
  (every? true? (map #(< v %) adj)))

(defn neighbors [data [x y]]
  (for [[dx dy] [[0 1] [0 -1] [-1 0] [1 0]]
        :let [[px py] [(+ x dx) (+ y dy)]]
        :when (and (>= px 0)
                   (< px (count (first data)))
                   (>= py 0)
                   (< py (count data)))]
    [(+ x dx) (+ y dy)]))

(defn neighbor-vals [data [x y]]
  (mapv #(get-in data (reverse %)) (neighbors data [x y])))

(defn low-points [data]
  (let [ymax (count data)
        xmax (count (first data))]
    (for [x (range 0 xmax)
          y (range 0 ymax)
          :let [v (get-in data [y x])
                adj (neighbor-vals data [x y])]
          :when (lowest? v adj)]
      [[x y] v])))

(defn part1 []
  (let [data (input)]
    (->> (low-points data)
         (map (comp inc second))
         (reduce +))))

(defn flow [data [x y]]
  (loop [visited #{[x y]}
         basin #{[x y]}
         [[cx cy :as cur] & to-visit] (neighbors data [x y])]

    (if (nil? cur)
      basin
      (let [npos (-> (neighbors data [x y]) (doto println))
            _ (println (->> npos (remove visited)))
            adj (->> npos
                     (remove visited)
                     (mapv #(get-in data (reverse %))))
            next-visited (conj visited cur)]
        (println visited basin cur to-visit adj)
        (if (lowest? (get-in data [cy cx]) adj)
          (recur next-visited
                 (conj basin cur)
                 (concat to-visit (->> (neighbors data cur) (remove visited))))
          (recur next-visited
                 basin
                 to-visit))))))

(defn cross [data [x y]]
  [(get-in data [y x])
   (neighbor-vals data [x y])])

(defn part2 []
  (let [data (input)
        basins (low-points data)]
    (->> (for [[[x y] _] basins]
           (flow data [x y]))
         (first))))
