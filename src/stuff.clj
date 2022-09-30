(ns stuff
  (:require [clojure.core.reducers :as r]
            [clojure.test :refer [deftest is]]))


(deftest a (is (=
                (r/fold + (r/filter even? (r/map inc [1 1 1 2])))
                (->> [1 1 1 2]
                     (r/map inc)
                     (r/filter even?)
                     (r/fold +)))))




