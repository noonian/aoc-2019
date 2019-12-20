(ns aoc.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.intcode :as intcode])
  (:refer-clojure :exclude [eval]))

(def input (slurp (io/resource "input/day2.txt")))
(def input-memory (intcode/parse-intcode input))

(defn set-inputs [memory noun verb]
  (assoc memory
    1 noun
    2 verb))

(defn alarm-state [memory]
  (set-inputs memory 12 2))

(defn find-inputs [memory desired-output]
  (first
   (for [noun (range 100)
         verb (range 100)
         :let [result (-> (intcode/eval (set-inputs memory noun verb))
                          (get-in [:memory 0]))]
         :when (= result desired-output)]
     [noun verb])))

(comment

  ;; part 1
  (-> (intcode/eval (alarm-state input-memory))
      (get-in [:memory 0])) ;4930687

  ;; part 2
  (let [[noun verb] (find-inputs input-memory 19690720)]
    (+ (* 100 noun) verb)) ;5335

  )
