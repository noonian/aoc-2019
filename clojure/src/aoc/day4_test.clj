(ns aoc.day4-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.day4 :as day4]))

(def input "248345-746315")

(deftest examples
  (is (day4/matches-criteria? 111111))
  (is (not (day4/matches-criteria? 223450)))
  (is (not (day4/matches-criteria? 123789))))

(deftest part-1
  (is (= 1019 (count (filter day4/matches-criteria? (day4/parse-range input))))))

(deftest part-2
  (is (= 660 (count (filter day4/matches-criteria-part-2? (day4/parse-range input))))))
