(ns aoc2017.day3
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

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
