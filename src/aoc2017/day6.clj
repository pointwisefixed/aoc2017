(ns aoc2017.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn redistribute [list]
  (let [max-value (apply max list)
        max-value-index (.indexOf list max-value)
        list-without-max (assoc list max-value-index 0)]
    (loop [current-index (+ max-value-index 1)
           current-list list-without-max
           left-to-distribute max-value]
      (let [index-to-use (cond
                           (>= current-index (count list)) 0
                           :else current-index)]
        (if (zero? left-to-distribute)
          current-list
          (let [current-index-val (get current-list index-to-use)
                left (- left-to-distribute 1)
                new-list (assoc current-list index-to-use (+ current-index-val 1))]
            (recur
              (+ index-to-use 1)
              new-list
              left)))))))

(defn day6-1 [original-list]
  (loop [current-list original-list
         history #{}
         rebalance-num 0]
    (let [redistributed-list (redistribute current-list)
          new-history (conj history redistributed-list)
          new-rebalance-num (+ rebalance-num 1)]
      (if (contains? history redistributed-list)
        new-rebalance-num
        (recur redistributed-list new-history new-rebalance-num)))))

(defn day6-2 [original-list]
  (loop [current-list original-list
         history '()
         rebalance-num 0
         last-time-seen 0]
    (let [redistributed-list (redistribute current-list)
          history-freq (frequencies history)
          new-history (conj history redistributed-list)
          found-value-frequency (get history-freq redistributed-list 0)
          new-last-time-seen (cond
                               (and (zero? last-time-seen) (= 1 found-value-frequency)) rebalance-num
                               :else last-time-seen)
          new-rebalance-num (+ 1 rebalance-num)]
      (if (> found-value-frequency 1)
        (- rebalance-num new-last-time-seen)
        (recur redistributed-list new-history new-rebalance-num new-last-time-seen)))))

(defn day6 [] (let [first-line (-> "input/day6" (io/resource) (slurp) (string/split-lines)
                                   (first) (string/split #"\s"))
                    num-list (-> (->> first-line (map #(Integer/parseInt %))) vec)]
                (println (day6-1 num-list))
                (println (day6-2 num-list))))
