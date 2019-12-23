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

(deftest day9-examples
  (is (= [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]
         (->> (intcode/program-states (intcode/environment [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]))
              (map :output)
              (filter identity))))
  (let [n (:output (intcode/eval (intcode/parse-intcode "1102,34915192,34915192,7,4,7,99,0")))]
    (is (= 16 (count (str n))))
    (is (= 1219070632396864 n)))
  (is (= 1125899906842624
         (:output (intcode/eval (intcode/parse-intcode "104,1125899906842624,99"))))))

(deftest day9-part-1
  (is (= 3742852857 (:output (intcode/eval {:inputs [1]} intcode/boost-program)))))

(deftest day9-part-2
  (is (= 73439 (:output (intcode/eval {:inputs [2]} intcode/boost-program)))))

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
