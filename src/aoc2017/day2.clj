(ns aoc2017.day2
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

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
