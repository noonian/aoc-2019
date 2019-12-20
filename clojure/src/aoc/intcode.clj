(ns aoc.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]])
  (:refer-clojure :exclude [eval]))

(defn parse-intcode [s]
  (mapv #(Integer/parseInt %) (str/split (str/trim s) #",")))

(defn instruction [impl arity]
  {:arity arity
   :impl impl})

(defn set-input [memory value]
  (assoc memory 1 value))

(defn set-inputs [memory noun verb]
  (assoc memory
    1 noun
    2 verb))

(defn inputs [memory] (take 2 (drop 1 memory)))
(defn output [memory] (first memory))
(defn set-input [memory input] (assoc memory 1 input))
(defn set-output [memory output] (assoc memory 0 output))

(def position-mode? (partial = 0))
(def immediate-mode? (partial = 1))

(defn load-param [memory value param-mode]
  (if (immediate-mode? param-mode)
    value
    (get memory value)))

(defn load-params [{:keys [ip memory param-modes]} arity]
  (into []
    (for [i (range arity)]
      (load-param memory (get memory (+ ip 1 i)) (get param-modes i 0)))))

(defn parse-op [ip memory #_{:keys [memory ip]}]
  (let [op-val (get memory ip)
        digits (->> op-val str reverse (map #(Integer/parseInt (str %))))
        opcode (Integer/parseInt (apply str (reverse (take 2 digits))))
        param-modes (into [] (drop 2 digits))]
    {:opcode opcode
     :param-modes param-modes}))

(defn find-op [{:keys [operations memory ip]}]
  (try
    (let [op-val (get memory ip)
          digits (->> op-val str reverse (map #(Integer/parseInt (str %))))
          opcode (Integer/parseInt (apply str (reverse (take 2 digits))))
          halt? (= 99 opcode)]
      (if (= opcode 99)
        {:opcode 99
         :halt? true}
        (let [{:keys [arity impl] :as op} (get operations opcode)
              _ (println "arity, impl:" arity impl)
              param-modes (into [] (drop 2 digits))
              _ (println "param-modes:" param-modes)
              params (into []
                       (for [i (range arity)]
                         (load-param memory (get memory (+ ip 1 i)) (get param-modes i 0))))]
          (merge {:params params
                  :opcode opcode}
                 op))))
    (catch Exception e
      (println "caught exception in find-op"))))

(defn add-instr [{:keys [ip memory param-modes] :as env}]
  (let [[a b] (load-params env 2)
        result-index (get memory (+ ip 3))]
    (-> env
        (update :ip + 4)
        (assoc-in [:memory result-index] (+ a b)))))

(defn multiply-instr [{:keys [ip memory param-modes] :as env}]
  (let [[a b] (load-params env 2)
        result-index (get memory (+ ip 3))]
    (-> env
        (update :ip + 4)
        (assoc-in [:memory result-index] (* a b)))))

(defn output-instr [env]
  (let [[value] (load-params env 1)]
    (-> env
        (update :ip + 2)
        (assoc :output value))))

(defn save-instr [{:keys [ip memory] :as env}]
  (let [value (Integer/parseInt (read-line))
        address (get memory (inc ip))]
    (-> env
        (update :ip + 2)
        (update :memory assoc address value))))

(defn jump-if-true [env]
  (let [[a b] (load-params env 2)]
    (if (not (zero? a))
      (assoc env :ip b)
      (update env :ip + 3))))

(defn jump-if-false [env]
  (let [[a b] (load-params env 2)]
    (if (zero? a)
      (assoc env :ip b)
      (update env :ip + 3))))

(defn less-than [{:keys [ip memory] :as env}]
  (let [[a b] (load-params env 2)
        result-index (get memory (+ ip 3))]
    (-> env
        (assoc-in [:memory result-index] (if (< a b) 1 0))
        (update :ip + 4))))

(defn equals [{:keys [ip memory] :as env}]
  (let [[a b] (load-params env 2)
        result-index (get memory (+ ip 3))]
    (-> env
        (assoc-in [:memory result-index] (if (= a b) 1 0))
        (update :ip + 4))))

(def operations2
  {1 add-instr
   2 multiply-instr
   3 save-instr
   4 output-instr
   5 jump-if-true
   6 jump-if-false
   7 less-than
   8 equals})

(defn environment [initial-memory]
  {:memory initial-memory
   :ip 0
   :operations operations2
   :mutating-ip? true})

(defn print-env [{:keys [memory ip halted? opcode param-modes output]}]
  (println
   (format "
instruction-pointer: %s
opcode: %s
param-modes: %s
output: %s
"
           ip
           opcode
           (str/join ", " param-modes)
           output)))

(defn process-instruction [{:keys [operations memory ip halted? mutating-ip?] :as initial-env}]
  (try
    (let [{:keys [opcode] :as env} (merge initial-env (parse-op ip memory))]
      (try
        (if (= 99 opcode)
          (assoc env :halted? true)
          (if-let [op (get operations opcode)]
            (op (dissoc env :output))
            (throw (Exception. (format "No operation found for opcode %s" opcode)))))
        (catch Exception e
          (println "caught inner exception in process-instruction processing opcode" opcode)
          (pprint env)
          (.printStackTrace e)
          (print-env env)
          {:halted? true})))
    (catch Exception e
      (println "caught exception in process-instruction")
      {:halted? true})))

(defn program-states [env]
  (into [] (take-while (comp not :halted?) (iterate process-instruction env))))

(defn eval [initial-memory]
  (last (program-states (environment initial-memory))))

(def input (slurp (io/resource "input/day5.txt")))
(def diagnostic-program (parse-intcode input))

(defn run-diagnostic []
  (let [states (program-states (environment diagnostic-program))]
    (doseq [[{:keys [ip opcode param-modes]} {:keys [output]}] (filter (fn [[ _ s]] (:output s)) (partition 2 1 (butlast states)))]
      (println (format "Testing opcode %s with param-modes %s at instruction pointer %s... output code: %s"
                       opcode
                       (str/join "," param-modes)
                       ip output)))
    (println "Diagnostic program output code:" (:output (last states)))))

(comment

  (run-diagnostic)

  )
