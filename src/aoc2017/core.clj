(ns aoc2017.core
  (:gen-class)
  (require [clojure.java.io :as io]))


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
    (reduce + (map (fn[[fst lst]] (cond (= fst lst) (+ fst lst) :else 0)) col-ordered))))

(defn -main
  "AOC YAY"
  [& args]
  (let [input-list-text (slurp (io/resource "input/day1"))
        input-list (map #(Character/getNumericValue %) (seq input-list-text))]
    (day1-1 input-list)
    (day1-2 input-list)))
