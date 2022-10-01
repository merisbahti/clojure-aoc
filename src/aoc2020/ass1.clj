(ns aoc2020.ass1
  (:require [clojure.core]
            [clojure.core.reducers :refer [fold]]
            [clojure.test :refer [deftest is]]))

(defn sum [xs] (fold + xs))
(deftest sumtest (is (= 6 (sum [1 2 3]))))

(defn mul [xs]  (fold * xs))
(deftest multest (is (= 6 (mul [1 2 3]))))

(defn all-pairs [coll]
  (let [x (first coll) xs (next coll)]
    (when xs
      (lazy-cat
       (map (fn [y] [x y]) xs)
       (all-pairs xs)))))
(deftest allpairstest (is (= (all-pairs [1 2 3]) [[1 2] [1 3] [2 3]])))

(defn all-triples [coll]
  (let [x (first coll) xs  (next coll)]
    (when xs
      (lazy-cat
       (map (fn [pairs] (cons x pairs)) (all-pairs xs))
       (all-triples xs)))))
(deftest alltriplestest (is (= (all-triples [1 2 3 4]) [[1 2 3] [1 2 4] [1 3 4] [2 3 4]])))

(defn filternot2020 [input] (filter #(= 2020 (sum %)) input))
(deftest filternot2020-test (is (= '([1010 1010] [1020 1000]) (filternot2020 [[1010 1010] [20] [1020 1000]]))))

(defn sol1 [input] (->> input
                        (all-pairs)
                        (filternot2020)
                        (first)
                        (mul)))

(defn sol1b [input] (->> input
                         (all-triples)
                         (filternot2020)
                         (first)
                         (mul)))

(def testinput [1721
                979
                366
                299
                675
                1456])

(deftest test-input (is (= 514579 (sol1 testinput))))
(def ass1lines (.split (clojure.core/slurp "input/a1/a.txt") "\n"))
(def ass1parsed (map #(Integer/parseInt %) ass1lines))
(comment ass1parsed)
(comment (sol1 ass1parsed))
(comment (sol1b ass1parsed))

(comment ass2parsed)
(comment (sol1 ass1parsed))