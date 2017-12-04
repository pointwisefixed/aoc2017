(ns aoc2017.core
  (:gen-class)
  (require [clojure.java.io :as io])
  (require [clojure.string :as string]))


(defn day1-1 [input-as-list]
  (loop [sum 0 prev-list '() current input-as-list]
    (if (empty? current)
      sum
      (recur (cond
               (or (= (first prev-list) (first current))    ;; first case 
                   (and (= (first current) (last prev-list)) (empty? (rest current))))
               (+ sum (first current))
               :else sum) (cons (first current) prev-list) (rest current)))))

(defn day1-2 [input-as-list]
  (let [
        halfway (/ (count input-as-list) 2)
        part (mapv vec (split-at halfway input-as-list))
        col-ordered (apply map vector part)]
    (reduce + (map (fn [[fst lst]] (cond (= fst lst) (+ fst lst) :else 0)) col-ordered))))

(defn day1 []
  (let [input-list-text (slurp (io/resource "input/day1"))
        input-list (map #(Character/getNumericValue %) (seq input-list-text))]
    (println (day1-1 input-list))
    (println (day1-2 input-list))))

(defn day2-1 [input-matrix]
  (let [matrix-of-differences (map (fn [lst] (- (apply max lst) (apply min lst))) input-matrix)
        checksum (reduce + matrix-of-differences)]
    checksum))

(defn is-divisible [num list] (let [possible (filter #(= (mod num %) 0) list)] possible))

(defn find-divisibles [list]
  (loop [test-number (first list)
         rest-of-list (rest list)]
    (let [
          next-test-number (first rest-of-list)
          next-rest-of-list (flatten (conj (vec (rest rest-of-list)) test-number))
          divisible-found (is-divisible test-number rest-of-list)]
      (if (not (empty? divisible-found))
        (cons test-number divisible-found)
        (recur next-test-number next-rest-of-list)))))

(defn day2-2 [input-matrix]
  (let [matrix-of-divisible (map find-divisibles input-matrix)
        matrix-of-divided (map (fn [lst] (/ (first lst) (second lst))) matrix-of-divisible)]
    (reduce + matrix-of-divided)))

(defn day2 []
  (let [lines (string/split-lines (slurp (io/resource "input/day2")))
        input-matrix (map (fn [line] (map #(Integer. %) (string/split line #"\s")))
                          lines)]
    (println (day2-1 input-matrix))
    (println (day2-2 input-matrix))))

(defn manhattan-dist [p1 p2 q1 q2] (+ (Math/abs (- p1 q1)) (Math/abs (- p2 q2))))

(defn move [x y dir]
  (cond
    (= dir :right) {:x (+ 1 x), :y y}
    (= dir :up) {:x x, :y (+ 1 y)}
    (= dir :left) {:x (- x 1), :y y}
    (= dir :down) {:x x, :y (- y 1)}))

(defn in-grid [grid coord-to-find]
  (let [x-val (:x coord-to-find)
        y-val (:y coord-to-find)]
    ;(println (str "contains? x:" x-val ", y: " y-val " in grid " grid))
    (not= 0 (aget grid x-val y-val))))

(defn find-next-move [grid x y curr-move]
  (let [go-up (move x y :up)
        go-left (move x y :left)
        go-down (move x y :down)
        go-right (move x y :right)]
    (cond
      (= curr-move :source) :right
      (and (= curr-move :right) (not (in-grid grid go-up))) :up
      (and (= curr-move :right) (in-grid grid go-up)) :right
      (and (= curr-move :up) (not (in-grid grid go-left))) :left
      (and (= curr-move :up) (in-grid grid go-left)) :up
      (and (= curr-move :left) (not (in-grid grid go-down))) :down
      (and (= curr-move :left) (in-grid grid go-down)) :left
      (and (= curr-move :down) (not (in-grid grid go-right))) :right
      (and (= curr-move :down) (in-grid grid go-right)) :down)))

(defn create-spiral-grid [input]
  (let [dim (+ (int (Math/sqrt input)) 5)]
    (println (str "grid dim " dim))
    (loop [grid (make-array Long/TYPE dim dim)
           x (quot dim 2)
           y (quot dim 2)
           val 1
           curr-move :source]
      (if (= input val)
        {:dim dim, :x x, :y y}
        (let [next-move (find-next-move grid x y curr-move)
              next-coord (move x y next-move)]
          (aset grid x y val)
          (recur grid
                 (:x next-coord)
                 (:y next-coord)
                 (+ val 1)
                 next-move))))))

(defn sum-adjacent-vals [grid val coord]
  (let [x (:x coord)
        y (:y coord)]
    (+ (aget grid (+ x 1) y)                                ;; right
       (aget grid (+ x 1) (+ y 1))
       (aget grid x (+ y 1))
       (aget grid (- x 1) (+ y 1))
       (aget grid (- x 1) y)
       (aget grid (- x 1) (- y 1))
       (aget grid x (- y 1))
       (aget grid (+ x 1) (- y 1)))))

(defn create-spiral-grid-2 [input]
  (let [dim (+ (int (Math/sqrt input)) 5)]
    (println (str "grid dim " dim))
    (loop [grid (make-array Long/TYPE dim dim)
           x (quot dim 2)
           y (quot dim 2)
           val 1
           curr-move :source]
      (if (> val input)
        val
        (let [next-move (find-next-move grid x y curr-move)
              next-coord (move x y next-move)]
          (aset grid x y val)
          (recur grid
                 (:x next-coord)
                 (:y next-coord)
                 (sum-adjacent-vals grid val next-coord)
                 next-move))))))
(defn is-valid [line]
  (empty? (filter (fn [[x freq]] (> freq 1)) (frequencies line))))

(defn is-valid-with-anagram [line]
  (let [sorted-line (map sort line)]
    (empty? (filter (fn [[x freq]] (> freq 1)) (frequencies sorted-line)))))

(defn day4-1 [lines]
  (count (filter is-valid lines)))

(defn day4-2 [lines]
  (count (filter is-valid-with-anagram lines)))

(defn day4 []
  (let [lines-text (string/split-lines (slurp (io/resource "input/day4")))
        lines (map #(string/split % #"\s") lines-text)]
    (println (str "# of lines " (count lines)))
    (println (day4-1 lines))
    (println (day4-2 lines))))

(defn day3-2 [input]
  (let [val (create-spiral-grid-2 input)]
    ;(println input-coord)
    val))

(defn day3-1 [input]
  (let [input-coord (create-spiral-grid input)
        xcoord (:x input-coord)
        ycoord (:y input-coord)
        dim (:dim input-coord)]
    ;(println input-coord)
    (manhattan-dist xcoord ycoord (quot dim 2) (quot dim 2))))

(defn -main
  "AOC YAY"
  [& args]
  (day4))
