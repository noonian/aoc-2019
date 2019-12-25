(ns aoc.day10-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc.day10 :as day10]))

;; .#..#
;; .....
;; #####
;; ....#
;; ...##

(def ex-simple
  (day10/parse-map ".#..#
.....
#####
....#
...##"))

(def ex-large
  (day10/parse-map
           ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"))

(deftest ansteroids-detected
  (is (= 7 (count (day10/asteroids-detected ex-simple [1 0]))))
  (is (= 6 (count (day10/asteroids-detected ex-simple [0 2]))))
  (is (= 8 (count (day10/asteroids-detected ex-simple [3 4]))))
  (is (= 7 (count (day10/asteroids-detected ex-simple [4 3])))))

(deftest best-loc
  (is (= [[5 8] 33]
         (day10/best-loc
          (day10/parse-map
           "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"))))
  (is (= [[1 2] 35]
         (day10/best-loc
          (day10/parse-map
           "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###."))))
  (is (= [[6 3] 41]
         (day10/best-loc
          (day10/parse-map
           ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#.."))))
  (is (= [[11 13] 210] (day10/best-loc ex-large))))

(def asteroid-map (day10/parse-map (str/trim (slurp (io/resource "input/day10.txt")))))

(deftest part-1
  (is (= [[8 16] 214] (day10/best-loc asteroid-map))))

(deftest vaporization-order
  (let [order (day10/vaporization-order ex-large [11 13])]
    (is (= [11 12] (first order)))
    (is (= [12 1] (second order)))
    (is (= [12 2] (get order 2)))
    (is (= [12 8] (get order 9)))
    (is (= [16 0] (get order 19)))
    (is (= [16 9] (get order 49)))
    (is (= [10 16] (get order 99)))
    (is (= [8 2] (get order 199)))
    (is (= [10 9] (get order 200)))
    (is (= [11 1] (get order 298)))))

(let [order (day10/vaporization-order asteroid-map [8 16])
      [x y] (get order 199)]
  (+ (* 100 x) y))
