(ns aoc.day3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.day3 :as day3]))

(def input (slurp (io/resource "input/day3.txt")))
(def input-wires (map day3/parse-wire (str/split-lines input)))

(deftest examples
  (is (= 159 (day3/solve (day3/parse-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                         (day3/parse-wire "U62,R66,U55,R34,D71,R55,D58,R83"))))
  (is (= 135 (day3/solve (day3/parse-wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
                         (day3/parse-wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))

(deftest part-1
  (is (= 865 (apply day3/solve input-wires))))

(deftest steps
  (let [w1 (day3/parse-wire "R8,U5,L5,D3")
        w2 (day3/parse-wire "U7,R6,D4,L4")]
    (println (day3/intersection-points w1 w2))
    (is (= 20 (day3/steps w1 [3 3])))
    (is (= 20 (day3/steps w2 [3 3])))
    (is (= 15 (day3/steps w1 [6 5])))
    (is (= 15 (day3/steps w2 [6 5]))))
  (let [w1 (day3/parse-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
        w2 (day3/parse-wire "U62,R66,U55,R34,D71,R55,D58,R83")]
    (is (= 610 (day3/fewest-combined-steps w1 w2))))
  (let [w1 (day3/parse-wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
        w2 (day3/parse-wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")]
    (is (= 410 (day3/fewest-combined-steps w1 w2)))))

#_(count (apply concat (:edges (first input-wires))))
#_(day3/fewest-combined-steps (first input-wires) (second input-wires))

(comment

  (time (println (apply fewest-combined-steps (map parse-wire input-wires))))
  ;; 35038
  ;; "Elapsed time: 220527.549807 msecs" !!!!!!

  )
