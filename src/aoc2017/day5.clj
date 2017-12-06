(ns aoc2017.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


;;result - steps-to-reach-exit
(defn day5-1 [list]
  (let [list-vec (vec list)
        list-size (count list-vec)]
    (loop [curr-list list-vec
           curr-index 0
           step 1]
      (let [curr-value (get curr-list curr-index)
            next-index (+ curr-index curr-value)]
        (if (>= next-index list-size)
          step
          (recur (assoc curr-list curr-index (+ curr-value 1)) next-index (+ step 1)))))))

(defn day5-2 [list]
  (let [list-vec (vec list)
        list-size (count list-vec)]
    (loop [curr-list list-vec
           curr-index 0
           step 1]
      (let [curr-value (get curr-list curr-index)
            next-index (+ curr-index curr-value)
            new-value (cond (>= curr-value 3) (- curr-value 1) :else (+ curr-value 1))]
        (if (>= next-index list-size)
          step
          (recur (assoc curr-list curr-index new-value) next-index (+ step 1)))))))

(defn day5 []
  (let [list (map #(Integer/parseInt %)
                  (string/split-lines (slurp (io/resource "input/day5"))))]
    (println (day5-1 list))
    (println (day5-2 list))))
