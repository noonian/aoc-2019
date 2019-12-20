(ns aoc.intcode-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [aoc.intcode :as intcode]
            [aoc.day2 :as day2]))

(deftest day2-examples2
  (is (= [3500 9 10 70 2 3 11 0 99 30 40 50]
         (:memory (intcode/eval [1 9 10 3 2 3 11 0 99 30 40 50]))))
  (is (= [2 0 0 0 99]
         (:memory (intcode/eval [1 0 0 0 99]))))
  (is (= [2 3 0 6 99]
         (:memory (intcode/eval [2 3 0 3 99]))))
  (is (= [30 1 1 4 2 5 6 0 99]
         (:memory (intcode/eval [1 1 1 4 99 5 6 0 99])))))

(def day2-input (slurp (io/resource "input/day2.txt")))
(def day2-input-memory (intcode/parse-intcode day2-input))

(deftest day2-part-1
  (is (= 4930687 (-> (intcode/eval (assoc day2-input-memory 1 12 2 2))
                     (get-in [:memory 0])))))

(deftest day2-part-2
  (is (= 5335 (let [[noun verb] (day2/find-inputs day2-input-memory 19690720)]
                (+ (* 100 noun) verb)))))

(deftest save-instruction
  (is (= [3 4 99 nil 4]
         (:memory
          (with-in-str "4"
            (intcode/eval [3 4 99 nil nil]))))))

(deftest day5-examples
  (is (= [1002 4 3 4 99]
         (:memory (intcode/eval [1002 4 3 4 33])))))

(def input (slurp (io/resource "input/day5.txt")))
(def input-memory (intcode/parse-intcode input))

(deftest day5-part-1
  (is (= 7692125 (:output (with-in-str "1" (intcode/eval input-memory))))))

(deftest day5-part-2
  (is (= 14340395 (:output (with-in-str "5" (intcode/eval input-memory))))))

(comment

  (def states
    (with-in-str "1"
      (intcode/program-states
       (intcode/environment input-memory))))

  (doseq [s states] (intcode/print-env s))

  (intcode/print-env (last states))

  (count states)
  (count input-memory)
  (pprint input-memory)

  )
