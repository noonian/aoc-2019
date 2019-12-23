(ns aoc.day7-test
  (:require [clojure.test :refer [deftest is]]
            [aoc.day7 :as day7]))

;; The examples answers seem to be wrong for this problem

(deftest part-1
  (is (= 45730 (day7/max-thruster-signal))))

(deftest part-2
  (is (= 5406484 (day7/max-thruster-signal-feedback))))
