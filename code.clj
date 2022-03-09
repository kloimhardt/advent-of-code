;; https://adventofcode.com/2021
;; login with my github account

(ns advent-of-code
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn readfile [name]
  (with-open [rdr (io/reader (io/file name))]
    (doall (line-seq rdr))))

;;day 1/1

(defn day01-1 [data]
  (->> (map - data (next data))
       (filter neg?)
       (count)))

;;day 1/2

(defn day01-2 [data]
  (let [windows (map + data (next data) (nnext data))]
    (->> (map - windows (next windows))
         (filter neg?)
         (count))))

(comment

  (def data (mapv parse-long (readfile "inputs/year2021/day1")))
  (day01-1 data) ;; => 1316
  (day01-2 data) ;; => 1344

  )

(comment

  (parse-long "1")
  (def data (mapv parse-long ["1" "-2" "3" "-4" "5"]))
  (next data)
  (nnext data)
  (filter neg? data)
  (->> data
       (filter neg?))
  (map + data data)
  (count data)

  )

;;day 2/1

(defn convert-text-into-keyword-and-number [text]
  (let [[_ cmd arg] (re-matches #"([a-z]+) ([0-9]+)" text)]
    [(keyword cmd) (parse-long arg)]))

(defn day02-1 [data]
  (let [[x y] (reduce
                (fn [[x y] [cmd arg]]
                  (case cmd
                    :forward [(+ x arg) y]
                    :down    [x (+ y arg)]
                    :up      [x (- y arg)]))
                [0 0] data)]
    (* x y)))

;; day 2/2

(defn day02-2 [data]
  (let [[x y _] (reduce
                  (fn [[x y aim] [cmd arg]]
                    (case cmd
                      :forward [(+ x arg) (+ y (* aim arg)) aim]
                      :down    [x y (+ aim arg)]
                      :up      [x y (- aim arg)]))
                  [0 0 0] data)]
    (* x y)))

(comment

  (def data (->> "inputs/year2021/day2"
                 readfile
                 (mapv convert-text-into-keyword-and-number)))
  (day02-1 data) ;; => 1893605
  (day02-2 data) ;; => 2120734350

  )

(comment

  (def data ["forward 4" "down 9" "forward 2" "forward 2" "down 7"])
  (re-matches #"([a-z]+) ([0-9]+)" "forward 4")
  (keyword "forward")

  )

;;day 3/1

(defn transpose [m]
  (apply map vector m))

(defn digits [lines]
  (->> lines
       transpose
       (map frequencies)
       (map (fn [{zeros \0 ones \1 :or {zeros 0 ones 0}}]
              (if (>= ones zeros) 1 0)))))

(defn day03-1 [lines]
  (let [digits (digits lines)
        invert-digits (mapv #(- 1 %) digits)
        γ      (Long/parseLong (apply str digits) 2)
        ε      (Long/parseLong (apply str invert-digits) 2)]
    (* γ ε)))

;;day 3/2

(defn most-common [lines & [invert?]]
  (loop [filtered-lines lines
         pos 0]
    (if (and (> (count filtered-lines) 1) (< pos (count (first lines))))
      (let [digit (nth (digits filtered-lines) pos)
            char-digit (-> (if invert? (- 1 digit) digit) str first)]
        (recur (filter #(= (nth % pos) char-digit) filtered-lines)
               (inc pos)))
      (Long/parseLong (first filtered-lines) 2))))

(defn least-common [lines]
  (most-common lines true))

(defn day03-2 [lines]
  (let [O₂   (most-common lines)
        CO₂  (least-common lines)]
    (* O₂ CO₂)))

(comment

  (def lines (readfile "inputs/year2021/day3"))
  (day03-1 lines) ;; => 4138664
  (day03-2 lines) ;; => 4273224

  )

(comment

  (def data "1001")
  (frequencies data)
  (transpose [[1 2] [3 4]])

  )

;;day 4 is perfect in day4.clj

;;day 5/1

(defn straight? [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2)))

(defn make-line [input-str]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input-str)]
    (map parse-long [x1 y1 x2 y2])))

(defn abs-range
  [a b]
  (cond
    (< a b) (range a (inc b))
    (> a b) (range a (dec b) -1)
    (= a b) (repeat a)))

(defn points
  [[x1 y1 x2 y2]]
  (map vector (abs-range x1 x2) (abs-range y1 y2)))

(defn day05-1 [input]
  (->> input
       (map make-line)
       (filter straight?)
       (map points)
       (apply concat)
       frequencies
       (filter (fn [[_point num-of-intersections]]
                 (> num-of-intersections 1)))
       count))

;;day 5/2

(defn day05-2 [input]
  (->> input
       (map make-line)
       (map points)
       (apply concat)
       frequencies
       (filter (fn [[_point num-of-intersections]]
                 (> num-of-intersections 1)))
       count))

(comment

  (def input (readfile "inputs/year2021/day5"))
  (day05-1 input) ;; => 5698
  (day05-2 input) ;; => 15463

  )

(comment

  ;;implement (frequencies (apply concat lines))

  (def lines
    [[[0 0] [1 1] [2 2]]
     [[0 0] [1 1] [2 2]]
     [[0 0] [0 1] [0 2]]])

  ;;procedural pseudo-code
  ;;graph = {}
  ;;foreach line in lines:
  ;;  foreach point in line:
  ;;    graph[point]+=1

  (letfn [(line [graph point]
            (update graph point #(inc (or % 0))))]
    (reduce #(reduce line %1 %2) {} lines))

  ;;but such a nesting of 'reduce' is avoided by '(apply concat ...)' in the first place

  )
