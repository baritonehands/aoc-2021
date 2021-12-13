(ns aoc.dec12
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn input []
  (->> (utils/day-file 12)
       ;(str/split-lines "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
       ;(str/split-lines "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")
       (mapv #(str/split % #"-"))))

(def terminal #{"start" "end"})

(defn create-graph [data]
  (reduce
    (fn [graph [from to]]
      (cond-> graph
          (not= from "end") (update (keyword from) (fnil conj #{}) (keyword to))
          (not= from "start") (update (keyword to) (fnil conj #{}) (keyword from))))
    {}
    data))

(defn small? [opt]
  (let [s (first (name opt))]
    (and (>= (.compareTo s \a) 0)
         (<= (.compareTo s \z) 0))))

(defn small-valid? [opt path small-cnt]
  (if (= small-cnt 1)
    (empty? (filter #(= % opt) path))
    (let [freqs (->> path
                     (filter small?)
                     (frequencies)
                     (vals)
                     (filterv #(>= % small-cnt)))]
      (or (empty? freqs)
          (= freqs [small-cnt])))))


(defn remove-invalid [opts path small-cnt]
  (set
    (filter
      (fn [opt]
        (or (not (small? opt))
            (small-valid? opt path small-cnt)))
      opts)))

(defn find-paths [graph small-cnt]
  (loop [incomplete (for [node (get graph :start)]
                      (conj [:start] node))
         paths #{}]
    (if (empty? incomplete)
      paths
      (let [next-paths (for [path incomplete
                             opt (-> (get graph (peek path))
                                     (remove-invalid path small-cnt))]
                         (conj path opt))]
        (recur (remove #(= (peek %) :end) next-paths)
               (set/union paths (set (filter #(= (peek %) :end) next-paths))))))))

(defn part1 []
  (let [data (create-graph (input))]
    (count (find-paths data 1))))

(defn part2 []
  (let [data (create-graph (input))]
    (count (find-paths data 2))))
