(ns aoc.dec22
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse-var [[_ v]]
  (let [[from to] (str/split v #"\.\.")]
    [(Long/parseLong from) (Long/parseLong to)]))

(defn parse-line [line]
  (let [[state coords] (str/split line #" ")]
    [(keyword state)
     (for [v (->> (str/split coords #","))
           :let [vrange (-> v (str/split #"=") (parse-var))]
           :when (seq vrange)]
       vrange)]))

(defn input []
  (for [line (utils/day-file 22)
        :let [[state coords] (parse-line line)]
        :when (= (count coords) 3)]
    [state coords]))

(defn part1 []
  (let [data (input)]
    (->> (for [[state [[xmin xmax] [ymin ymax] [zmin zmax]]] data
               x (range (max xmin -50) (min (inc xmax) 51))
               y (range (max ymin -50) (min (inc ymax) 51))
               z (range (max zmin -50) (min (inc zmax) 51))]
           [state [x y z]])
         (reduce
           (fn [cubes [state coord]]
             (if (= state :on)
               (conj cubes coord)
               (disj cubes coord)))
           #{})
         (count))))

(defn volume [{:keys [bounds vacuums]}]
  (let [[[xmin xmax] [ymin ymax] [zmin zmax]] bounds]
    (-> (* (inc (- xmax xmin))
           (inc (- ymax ymin))
           (inc (- zmax zmin)))
        (- (->> vacuums (map volume) (reduce +))))))

(defn intersect-ranges [[lmin lmax] [rmin rmax]]
  (if-not (or (> rmin lmax)
              (> lmin rmax))
    (->> [lmin lmax rmin rmax]
         (sort)
         (drop 1)
         (take 2))))

(defn intersect-bounds [[lx ly lz] [rx ry rz]]
  (let [x (intersect-ranges lx rx)
        y (intersect-ranges ly ry)
        z (intersect-ranges lz rz)]
    (if (every? some? [x y z])
      [x y z])))

(defn remove-cuboid [{:keys [bounds vacuums] :as cuboid} to-remove]
  (if-let [shaved (intersect-bounds bounds to-remove)]
    (assoc cuboid
      :vacuums (->> vacuums
                    (mapv #(remove-cuboid % shaved))
                    (cons {:bounds  shaved
                           :vacuums []})))
    cuboid))

(defn part2 []
  (let [data (input)]
    (loop [cuboids []
           [[state bounds] & more] data]
      (if-not state
        (->> cuboids (map volume) (reduce +))
        (let [next-cuboids (cond->> cuboids
                                    true (mapv #(remove-cuboid % bounds))
                                    (= state :on) (cons {:bounds  bounds
                                                         :vacuums []}))]
          (recur next-cuboids more))))))

