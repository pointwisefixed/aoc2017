(ns aoc2017.day1
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

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
