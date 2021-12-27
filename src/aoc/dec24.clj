(ns aoc.dec24
  (:require [aoc.utils :as utils]
            [clojure.string :as str])
  (:import (clojure.lang IFn ArityException)))

(defn parse-arg [arg]
  (try
    (Long/parseLong arg)
    (catch Exception _
      (keyword arg))))

(defn parse-instr [line]
  (let [[op v & [arg]] (str/split line #" ")
        args (if arg
               [(parse-arg arg)]
               [])]
    (into [(keyword op) (keyword v)] args)))

(defn input []
  (->> (utils/day-file 24)
       (mapv parse-instr)))

(defmulti alu (fn [state op _ _] op))

(defmethod alu :inp [{:keys [inputs] :as state} _ v _]
  (let [[input-val & more] inputs]
    (-> state
        (assoc v input-val)
        (assoc :inputs more))))

(defn var-or-literal [state v]
  (if (keyword? v)
    (get state v)
    v))

(defmethod alu :add [state _ a b]
  (update state a + (var-or-literal state b)))

(defmethod alu :mul [state _ a b]
  (update state a * (var-or-literal state b)))

(defmethod alu :div [state _ a b]
  (update state a #(int (/ (float %) (float (var-or-literal state b))))))

(defmethod alu :mod [state _ a b]
  (update state a mod (var-or-literal state b)))

(defmethod alu :eql [state _ a b]
  (update state a #(if (= % (var-or-literal state b)) 1 0)))

(defn starting-state [inputs]
  {:w 0 :x 0 :y 0 :z 0 :inputs inputs})

(defn int->input [n]
  (->> (str n)
       (mapv #(- (int %) (int \0)))))

(defn input->int [ds]
  (Long/parseLong (apply str ds)))

(defn run-program [program cache inputs]
  (println program inputs)
  (reduce
    (fn [{:keys [inputs z] :as state} snippet]
      ;(if-let [z-result (get @cache [(first inputs) z])]
      ;  (merge state {:inputs (drop 1 inputs)
      ;                :z      z-result})
      (let [{snippet-z :z
             :as       next-state} (reduce
                                     #(apply alu %1 %2)
                                     state
                                     snippet)]
        ;(swap! cache assoc [(first inputs) z] snippet-z)
        next-state))
    (starting-state inputs)
    program))

(defn iteration [program cache [idx input]]
  (let [result (run-program program cache input)]
    (when (or (zero? (mod idx 100000)))
      ;(< (get result :z) 11700000)
      ;(= (mod (input->int input) (get result :z)) 0))
      (println input result))
    (if (zero? (:z result))
      (println input))
    result))

(defn disassemble [program]
  (->> (for [[idx instr] (map-indexed vector (->> program
                                                  (partition-by #(= (first %) :inp))
                                                  (remove #(= (count %) 1))))]
         [idx
          (for [[op dest arg] instr]
            (str (name dest) " = " (name dest) " " (name op) " " arg))])
       (into {})))

(defn optimize [program]
  (vec (for [[inp instr] (->> program
                              (partition-by #(= (first %) :inp))
                              (partition 2))
             :let [[op v] (first inp)
                   _ (println inp)]]
         (cons #(alu % op v v)
               (for [[op arg1 arg2] instr]
                 #(alu % op arg1 arg2))))))

(defn part1 []
  (let [program (optimize (input))
        cache (atom {})]
    (->> (range 99999999999999 11111111111111 -1)
         (map int->input)
         (filter #(every? pos? %))
         (map vector (range))
         (pmap (partial iteration program cache))
         (filter #(zero? (:z %)))
         (first))))

(defn starting-state-recursive [z inputs]
  {:w 0 :x 0 :y 0 :z z :inputs inputs})

(defn run-program-recursive [state program]
  (println state program)
  (reduce
    (fn [state f]
      (f state))
    state
    program))

(defn step [cache [z inputs] snippets]
  (when (and (= (count inputs) 14)
             (= (mod (input->int inputs) 1000000) 111111))
    (println [z inputs] snippets))
  (let [snippet (first snippets)
        parallel? (= (count inputs) 6)]
    (if (= (count inputs) 14)
      (let [cached (get @cache [z (peek inputs)])
            {z-next :z} (if cached
                          cached
                          (run-program-recursive (starting-state-recursive z inputs) snippet))
            _ (if-not cached
                (swap! cache assoc [z (peek inputs)] {:z z-next}))]
        [(= z-next 0) (input->int inputs)])
      (->> (for [digit (range 9 0 -1)
                 :let [next-inputs (conj inputs digit)
                       cached (get @cache [z digit])
                       {z-next :z} (if cached
                                     cached
                                     (run-program-recursive (starting-state-recursive z next-inputs) snippet))
                       _ (if-not cached
                           (swap! cache assoc [z digit] {:z z-next}))]]
             (if parallel?
               (future (step cache [z-next next-inputs] (rest snippets)))
               (step cache [z-next next-inputs] (rest snippets))))
           (reduce
             (fn [[success? result] child]
               (if success?
                 [success? result]
                 (if parallel?
                   (deref child)
                   child)))
             [false nil])))))

(defn part1-recursive []
  (let [program (optimize (input))]
    (step (atom {}) [0 []] program)))


; clear x
; x = x + z
; x = x mod 26
; z = z / DZ
; x = x + AX
; x = x == input
; x = x == 0
; clear y
; y = y * x + 1
; z = z * y
; y = input
; y = y + AY
; y = y * x
; z = z + y

; def run(ch, z, w):
;	x = AX[ch] + (z % 26)
;	z = z // DZ[ch]
;	if x != w:
;		z *= 26
;		z += w + AY[ch]
;	return z

(def params
  {0  {:ax 11 :ay 16 :dz 1.}
   1  {:ax 12 :ay 11 :dz 1.}
   2  {:ax 13 :ay 12 :dz 1.}
   3  {:ax -5 :ay 12 :dz 26.}
   4  {:ax -3 :ay 12 :dz 26.}
   5  {:ax 14 :ay 2 :dz 1.}
   6  {:ax 15 :ay 11 :dz 1.}
   7  {:ax -16 :ay 4 :dz 26.}
   8  {:ax 14 :ay 12 :dz 1.}
   9  {:ax 15 :ay 9 :dz 1.}
   10 {:ax -7 :ay 10 :dz 26.}
   11 {:ax -11 :ay 11 :dz 26.}
   12 {:ax -6 :ay 6 :dz 26.}
   13 {:ax -11 :ay 15 :dz 26.}})

(defn generate-machine []
  (->> (for [idx (range 0 14)
             :let [{:keys [ax ay dz]} (get params idx)]]
         (fn [state]
           ;(println (into [] state))
           (aset state 1 (unchecked-add-int ax
                                            (unchecked-remainder-int (aget state 3) 26)))
           ;(println "x = ax + z % 26" (into [] state))
           (aset state 3 (int (/ (aget state 3) dz)))
           ;(println "z = z // dz" (into [] state))
           (when (not= (aget state 0) (aget state 1))
             (aset state 3 (unchecked-multiply-int (aget state 3) 26))
             ;(println "z *= 26" (into [] state))
             (aset state 3 (unchecked-add-int (aget state 0) ay)))))
       ;(println "z += w + ay" (into [] state)))))
       ;(println (into [] state))))
       (into-array IFn)))

(defn part1-fast []
  (let [fns (generate-machine)
        state (int-array 4 [0 0 0 0])
        cnt (int-array 1 [0])]
    (->> (for [i14 (range 9 0 -1)
               i13 (range 9 0 -1)
               i12 (range 9 0 -1)
               i11 (range 9 0 -1)
               i10 (range 9 0 -1)
               i9 (range 9 0 -1)
               i8 (range 9 0 -1)
               i7 (range 9 0 -1)
               i6 (range 9 0 -1)
               i5 (range 9 0 -1)
               i4 (range 9 0 -1)
               i3 (range 9 0 -1)
               i2 (range 9 0 -1)
               i1 (range 9 0 -1)
               :let [input (int-array 14 [i14 i13 i12 i11 i10 i9 i8 i7 i6 i5 i4 i3 i2 i1])]]
           (do
             (aset cnt 0 (inc (aget cnt 0)))
             (doseq [i (range 0 14)]
               (aset state 0 (aget input i))
               (aset state 1 0)
               (aset state 2 0)
               ((aget fns i) state))
             ;(println (into [] state)))
             (when (zero? (mod (aget cnt 0) 10000))
               (println (into [] input)
                        (into [] state)))
             (get state 3)))
         (filter #(= % 0)))))
;(first))))





