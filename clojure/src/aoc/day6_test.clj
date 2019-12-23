(ns aoc.day6-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [aoc.day6 :as day6]))

(def ex-orbits
  (day6/parse-orbits
   "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"))

(def input-orbits (day6/parse-orbits (slurp (io/resource "input/day6.txt"))))

(deftest orbits
  (is (= ["C" "B" "COM"] (day6/orbits ex-orbits "D")))
  (is (= ["K" "J" "E" "D" "C" "B" "COM"] (day6/orbits ex-orbits "L")))
  (is (= [] (day6/orbits ex-orbits "COM"))))

(deftest count-total-orbits
  (is (= 42 (day6/count-total-orbits ex-orbits))))

(deftest part-1
  (is (= 254447 (day6/count-total-orbits input-orbits))))

(def part-2-ex-orbits
  (assoc ex-orbits
    "YOU" "K"
    "SAN" "I"))

(deftest relevant-subbranches
  (is (= [["D" "E" "J" "K"]
          ["D" "I"]]
         (day6/relevant-subbranches (day6/orbits part-2-ex-orbits "YOU")
                                    (day6/orbits part-2-ex-orbits "SAN")))))

(deftest min-orbital-transfers
  (is (= 4 (day6/min-orbital-transfers part-2-ex-orbits "YOU" "SAN"))))

(deftest part-2
  (is (= 445 (day6/min-orbital-transfers input-orbits "YOU" "SAN"))))
