(ns stuff 
  (:require [clojure.test :refer [deftest is]]))

(def f #(+ % 1))
(deftest a (is (= 1 1)))