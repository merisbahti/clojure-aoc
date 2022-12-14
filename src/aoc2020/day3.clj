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

(defn jump [input x y dx dy]
  (let
   [width (count (first input))]
    [(mod (+ x dx) width) (+ y dy)]))
(deftest test-jump1 (is (= [3 1] (jump test-input 0 0 3 1))))
(deftest test-jump2 (is (= [2 1] (jump test-input 10 0 3 1))))
(deftest test-jump3 (is (= [3 1] (jump test-input 11 0 3 1))))

(defn has-tree? [input x y] (= "#" (str (get (get input y) x))))
(deftest test-has-tree1 (is (false? (has-tree? test-input 0 0))))
(deftest test-has-tree2 (is (true? (has-tree? test-input 2 0))))
(deftest test-has-tree3 (is (true? (has-tree? test-input 0 1))))

(defn sol1 [input dx dy]
  (loop [x 0 y 0 trees 0]
    (if (<= (count input) y)
      trees
      (let [[newX newY] (jump input x y dx dy)]
        (recur
         newX
         newY
         (if (has-tree? input x y) (+ trees 1) trees))))))

(deftest test-input-test (is (= 7 (sol1 test-input 3 1))))

(def sol2-slopes [[1 1], [3 1], [5 1], [7 1], [1 2]])
(deftest sol2-test-input
  (is (=
       336
       (reduce * (map #(sol1 test-input (first %) (second %)) sol2-slopes)))))
(def sol-input (parse-input (slurp "input/day3/input.txt")))
(comment sol-input)
(deftest sol2-test
  (is (=
       3952146825
       (reduce * (map #(sol1 sol-input (first %) (second %)) sol2-slopes)))))
