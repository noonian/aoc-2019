(ns aoc.day8-test
  (:require [clojure.test :refer [deftest is]]
            [aoc.day8 :as day8]))

(deftest build-layers
  (is (= [[[1 2 3] [4 5 6]]
          [[7 8 9] [0 1 2]]]
         (day8/build-layers 3 2 (day8/parse-digits "123456789012")))))

(deftest merge-layers
  (is (= [[0 1]
          [1 0]]
         (day8/merge-layers [[[0 2] [2 2]]
                             [[1 1] [2 2]]
                             [[2 2] [1 2]]
                             [[0 0] [0 0]]]))))

(deftest part-1
  (is (= 2480 (day8/checksum (day8/build-layers 25 6 day8/input-digits)))))

;; ZYBLH
(def part-2-answer
  "1111010001111001000010010
0001010001100101000010010
0010001010111001000011110
0100000100100101000010010
1000000100100101000010010
1111000100111001111010010")

(deftest part-2
  (is (= part-2-answer (day8/render-image (day8/build-layers 25 6 day8/input-digits)))))
