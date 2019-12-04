(ns aoc.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:refer-clojure :exclude [eval]))

(defn parse-intcode [s]
  (mapv #(Integer/parseInt %) (str/split (str/trim s) #",")))

(def input (slurp (io/resource "input/day2.txt")))
(def input-memory (parse-intcode input))

(defn instruction [impl arity]
  {:arity arity
   :impl impl})

(def operations
  {1 (instruction + 2)
   2 (instruction * 2)})

(defn eval [program]
  (loop [ip 0
         memory program]
    (let [opcode (get memory ip)]
      (if (= opcode 99)
        memory
        (let [{:keys [arity impl]} (get operations opcode)
              others (drop (inc ip) memory)
              params (mapv memory (take arity others))
              result-index (get memory (+ ip arity 1))
              result-val (apply impl params)]
          (recur (+ ip arity 2)
                 (assoc memory result-index result-val)))))))

(defn set-inputs [memory noun verb]
  (assoc memory
    1 noun
    2 verb))

(defn output [memory] (first memory))

(defn alarm-state [memory]
  (set-inputs memory 12 2))

(defn find-inputs [memory desired-output]
  (->> (for [noun (range 100)
             verb (range 100)]
         [noun verb (output (eval (set-inputs memory noun verb)))])
       (filter (fn [[noun verb result]] (= result desired-output)))
       first))

(comment

  (eval (parse-intcode "1,9,10,3,2,3,11,0,99,30,40,50"))

  ;; part 1
  (output (eval (alarm-state input-memory))) ;4930687

  ;; part 2
  (let [[noun verb] (find-inputs input-memory 19690720)]
    (+ (* 100 noun) verb)) ;5335

  )
