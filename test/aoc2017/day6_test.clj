(ns aoc2017.day6-test
  (:require [clojure.test :refer :all]
            [aoc2017.day6 :refer :all]))

(deftest day6test
  (testing "day6-silver"
    (is (= (day6-1 [0 2 7 0]) 5)))
  (testing "day6-gold"
    (is (= (day6-2 [0 2 7 0]) 4))))
