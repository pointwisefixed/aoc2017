(ns aoc2017.core
  (:gen-class)
  (require [clojure.java.io :as io])
  (require [clojure.string :as string]))


(defn day1-1 [input-as-list]
  (loop [sum 0 prev-list '() current input-as-list]
    (if (empty? current)
      sum
      (let [prev (first prev-list)
            curr (first current)
            new-prev-list (cons curr prev-list)
            new-current (rest current)
            new-sum
            (cond
              (or (= prev curr)                             ;; first case 
                  (and (= curr (last prev-list)) (empty? new-current)))
              (+ sum curr)
              :else sum)]
        (recur new-sum new-prev-list new-current)))))

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
    (println test-number)
    (println rest-of-list)
    (let [
          next-test-number (first rest-of-list)
          next-rest-of-list (flatten (conj (vec (rest rest-of-list)) test-number))
          divisible-found (is-divisible test-number rest-of-list)]
      (println (str "next test " next-test-number))
      (println (str "next list " (vec next-rest-of-list)))
      (println (str "divisible list" (vec divisible-found)))
      (println (str "divisible not empty?" (not (empty? divisible-found))))
      (if (not (empty? divisible-found))
        (cons test-number divisible-found)
        (recur next-test-number next-rest-of-list)))))

(defn day2-2 [input-matrix]
  (let [matrix-of-divisible (map find-divisibles input-matrix)
        matrix-of-divided (map (fn[lst] (/ (first lst) (second lst))) matrix-of-divisible)]
    (println matrix-of-divided)
    (reduce + matrix-of-divided)))



(defn day2 []
  (let [lines (string/split-lines (slurp (io/resource "input/day2")))
        input-matrix (map (fn [line] (map #(Integer. %) (string/split line #"\s")))
                          lines)]
    (println (day2-1 input-matrix))
    (println (day2-2 input-matrix))))


(defn -main
  "AOC YAY"
  [& args] (day2))
