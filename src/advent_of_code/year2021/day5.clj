(ns advent-of-code.year2021.day5
  (:require
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.string :as string]
   #_[clj-async-profiler.core :as profiler]))

(defrecord Vent [x1 y1 x2 y2])

(defn parse-vent [s]
  (let [[x1 y1 x2 y2] (->> (re-matches #"\s*(\d+)\s*,\s*(\d+)\s*->\s*(\d+)\s*,\s*(\d+)\s*" s)
                           (next)
                           (mapv parse-long))]
    (Vent. x1 y1 x2 y2)))

(defn parse [input]
  (-> input
      (str/trim)
      (str/split #"\n")
      (->> (mapv parse-vent))))

(def example
  (slurp "inputs/year2021/day5.example"))

(def input
  (slurp "inputs/year2021/day5"))

(defn sign ^long [^long x]
  (cond
    (> x 0) 1
    (< x 0) -1
    (== x 0) 0))

(defn vent-seq [vent]
  (let [{:keys [^long x1 ^long x2 ^long y1 ^long y2]} vent
        steps (max (math/abs (- x2 x1)) (math/abs (- y2 y1)))
        dx    (sign (- x2 x1))
        dy    (sign (- y2 y1))]
    (mapv #(vector (+ x1 (* dx %)) (+ y1 (* dy %))) (range (inc steps)))))

(defn solve [vents]
  (let [width   (inc (reduce max (mapcat (juxt :x1 :x2) vents)))
        height  (inc (reduce max (mapcat (juxt :y1 :y2) vents)))
        matrix  ^longs (make-array Long/TYPE (* height width))
        xy->idx (fn [x y] (+ x (* y width)))
        *res    (volatile! 0)]
    (doseq [vent vents
            [x y] (vent-seq vent)
            :let [val (aget matrix (xy->idx x y))]]
      (aset matrix (xy->idx x y) (inc val))
      (when (== val 1)
        (vswap! *res inc)))
    @*res))

(defn part1
  ([] (part1 input))
  ([input]
   (solve (->> (parse input)
               (filter #(or (= (:x1 %) (:x2 %)) (= (:y1 %) (:y2 %))))))))

(defn part2
  ([] (part2 input))
  ([input]
   (solve (parse input))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day5 :reload))
  (part1 example)
  (part1)
  (profiler/profile (part1))
  (part2 example)
  (part2))

;;--------- solution https://github.com/neilcode/advent-of-code-2021/blob/main/src/calendar/2021/day_05.clj

(defn straight? [{{x' :x y' :y} :source {x :x y :y} :dest :as line}] (or (= x' x) (= y' y)))

(defn make-line [input-str]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input-str)]
    {:source {:x (Integer/parseInt x1) :y (Integer/parseInt y1)}
     :dest   {:x (Integer/parseInt x2) :y (Integer/parseInt y2)}}))

(defn abs-range
  "Generates a range between two points on a line regardless of the order in which the points are given"
  [a b]
  (cond
    (< a b) (range a (inc b))    ;; incrementing range
    (> a b) (range a (dec b) -1) ;; decrementing range
    (= a b) (repeat a)))         ;; infinite range :D

(defn points
  "Returns a collection of [x y] coordinates that comprise a given line"
  [{{x' :x y' :y} :source {x :x y :y} :dest :as line}]
  (map vector (abs-range x' x) (abs-range y' y)))

(comment
  (points {:source {:x 692, :y 826}, :dest {:x 692, :y 915}}))

(defn xy->intersections
  ([line] (xy->intersections line {}))
  ([line graph]
   (reduce (fn [graph point] (update graph point #(inc (or % 0))))
           graph
           (points line))))

(defn graph-lines
  ([lines] (graph-lines {} lines))
  ([graph lines] (reduce #(xy->intersections %2 %1) graph lines)))

(defn part-one
  [input]
  (->> (map make-line input)
       (filter straight?)
       graph-lines
       (filter (fn [[_point num-of-intersections]]
                 (> num-of-intersections 1)))
       count))

(defn part-two
  [input]
  (->> (map make-line input)
       graph-lines
       (filter (fn [[_point num-of-intersections]]
                 (> num-of-intersections 1)))
       count))

(defn each-line
  "Returns a list of strings representing lines in the given file"
  [filename & {:keys [root path matcher mapfn]
               :or
               {root (System/getProperty "user.dir")
                path "resources/"
                matcher #"\n"
                mapfn identity}}]
  (as-> (str root "/" path filename) $input
    (slurp $input)
    (string/split $input matcher)
    (map mapfn $input)))

(def input-new (each-line "day5" :path "inputs/year2021/"))

(= (part-one input-new) 5698)
(= (part-two input-new) 15463)
