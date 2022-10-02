(ns aoc2020.day3
  (:require [clojure.test :refer [deftest is]]))

(def test-input-raw "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn parse-input [input] (.split input "\n"))
(deftest parse-input-test (is (= 11 (count (parse-input test-input-raw)))))
(def test-input (parse-input test-input-raw))

(defn jump [input x y]
  (let
   [dx 3
    dy 1
    width (count (first input))]
    [(mod (+ x dx) width) (+ y dy)]))
(deftest test-jump1 (is (= [3 1] (jump test-input 0 0))))
(deftest test-jump2 (is (= [2 1] (jump test-input 10 0))))
(deftest test-jump3 (is (= [3 1] (jump test-input 11 0))))

(defn has-tree? [input x y] (= "#" (str (get (get input y) x))))
(deftest test-has-tree1 (is (false? (has-tree? test-input 0 0))))
(deftest test-has-tree2 (is (true? (has-tree? test-input 2 0))))
(deftest test-has-tree3 (is (true? (has-tree? test-input 0 1))))

(def dx 3)
(defn sol1 [input]
  (loop [x 0
         y 0
         trees 0]
    (println x y trees)
    (if (<= (count input) y)
      trees
      (recur
       (mod (+ x dx) (count (first input)))
       (+ y 1)
       (if (has-tree? input x y) (+ trees 1) trees)))))

(deftest test-input-test (is (= 7 (sol1 test-input))))