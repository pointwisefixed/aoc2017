(ns aoc2017.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

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

