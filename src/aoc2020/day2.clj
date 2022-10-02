(ns aoc2020.day2
  (:require [clojure.test :refer [deftest is]]))

(def test-input "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defn parse-line [line]
  (let [[_ min max letter pass] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    [(Integer/parseInt min) (Integer/parseInt max) letter pass]))
(deftest parse-line-test (is (= [1 3 "a" "abcde"] (parse-line "1-3 a: abcde"))))

(defn parse-input [text] (map parse-line (.split text "\n")))
(deftest parse-input-test (is (= [[1 3 "a" "abcde"] [1 3 "b" "cdefg"] [2 9 "c" "ccccccccc"]] (parse-input test-input))))

(defn count-occurence [letter text] (->> ""
                                         (.split text)
                                         (filter #(= % letter))
                                         count))
(deftest count-occurence-test (is (= 5 (count-occurence "a" "ababababa"))))

(defn is-valid [[min max letter pass]]
  (let [occurences (count-occurence letter pass)]
    (and (<= occurences max) (>= occurences min))))
(deftest is-valid-test (is (= true (is-valid [1 3 "a" "abcde"]))))
(deftest is-invalid-test (is (= false (is-valid [2 3 "a" "abcde"]))))

(defn sol1 [input] (map is-valid (parse-input input)))
(deftest test-testinput (is (= [true false true] (sol1 test-input))))

(comment  (count (filter #(= true  %) (sol1 (slurp "input/day2/a.txt")))))

(defn is-valid-2 [[index1 index2 letter pass]]
  (let [match-index2 (= (str (get pass (- index2 1))) letter)
        match-index1 (= (str (get pass (- index1 1))) letter)
        valid (= 1 (count (filter #(= true %) [match-index1 match-index2])))]
    valid))
(deftest is-valid-2-test-1 (is (= true (is-valid-2 [1 3 "a" "abcde"]))))
(deftest is-valid-2-test-2 (is (= false (is-valid-2 [1 3 "b" "cdefg"]))))
(deftest is-valid-2-test-3 (is (= false (is-valid-2 [2 9 "c" "ccccccccc"]))))
(deftest is-valid-2-test-4 (is (= false (is-valid-2 [1 3 "w" "wwwncwwwkwfww"]))))

(comment (count
          (filter #(= true %)
                  (map is-valid-2 (parse-input (slurp "input/day2/a.txt"))))))