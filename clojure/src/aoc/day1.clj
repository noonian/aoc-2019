(ns aoc.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "input/day1.txt")))
(def module-masses (mapv #(Integer/parseInt %) (str/split-lines input)))

(defn fuel-for-module [mass]
  (- (quot mass 3) 2))

(defn total-fuel [compute-fuel module-masses]
  (reduce + 0 (map compute-fuel module-masses)))

;; part 2

(defn fuel-for-module2 [mass]
  (loop [m mass
         result 0]
    (let [fuel (fuel-for-module m)]
      (if (neg? fuel)
        result
        (recur fuel (+ fuel result))))))

;; (pos? 1)

(comment

  ;; part 1
  (total-fuel fuel-for-module module-masses) ;3415076

  ;; part 2
  (total-fuel fuel-for-module2 module-masses) ;5119745

  )
