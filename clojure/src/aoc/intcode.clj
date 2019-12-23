(ns aoc.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go chan]]
            [clojure.pprint :refer [pprint]])
  (:refer-clojure :exclude [eval])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)))

(defn parse-intcode [s]
  (mapv #(Integer/parseInt %) (str/split (str/trim s) #",")))

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

(defn output-instr [{:keys [out-c] :as env}]
  (let [[value] (load-params env 1)]
    (when out-c
      (async/put! out-c value))
    (-> env
        (update :ip + 2)
        (assoc :output value))))

(defmulti get-input-value
  (fn [{:keys [inputs]}]
    (cond
      (coll? inputs) :collection
      (instance? ManyToManyChannel inputs) :channel
      :else :read-line)))

(defmethod get-input-value :collection [{:keys [inputs] :as env}]
  [(first inputs) (update env :inputs (partial drop 1))])

(defmethod get-input-value :read-line [env]
  [(Integer/parseInt (read-line)) env])

(defmethod get-input-value :channel [{:keys [inputs] :as env}]
  (println "channel method called")
  [(async/<!! inputs) env])

#_(defn get-input-value [{:keys [inputs]}]
    #_(println "inputs:" inputs)
    (or (first inputs) (Integer/parseInt (read-line))))

(defn input-instr [{:keys [ip memory] :as env}]
  (let [[input-value new-env] (get-input-value env)
        _ (println "recieved input value:" input-value)
        address (get memory (inc ip))]
    (-> new-env
        (update :ip + 2)
        ;; (update :inputs (partial drop 1))
        (update :memory assoc address input-value))))

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
   3 input-instr
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

(defn print-env [{:keys [memory ip halted? opcode param-modes output inputs]}]
  (println
   (format "
instruction-pointer: %s
opcode: %s
param-modes: %s
inputs: %s
output: %s
"
           ip
           opcode
           (str/join ", " param-modes)
           (str/join "," inputs)
           output)))

(defn halt! [{:keys [out-c] :as env}]
  (when out-c
    (async/close! out-c))
  (assoc env :halted? true))

(defn process-instruction [{:keys [operations memory ip halted? mutating-ip? out-c] :as initial-env}]
  (try
    (let [{:keys [opcode] :as env} (merge initial-env (parse-op ip memory))]
      (try
        (if (= 99 opcode)
          (halt! env)
          (if-let [op (get operations opcode)]
            (op (dissoc env :output))
            (throw (Exception. (format "No operation found for opcode %s" opcode)))))
        (catch Exception e
          (println "caught inner exception in process-instruction processing opcode" opcode)
          (pprint env)
          (.printStackTrace e)
          (print-env env)
          (halt! env))))
    (catch Exception e
      (println "caught exception in process-instruction")
      (halt! initial-env))))

(defn program-states [env]
  (into [] (take-while (comp not :halted?) (iterate process-instruction env))))

(defn eval-env [env]
  (last (program-states env)))

(defn eva
  ([initial-memory] (eval nil initial-memory))
  ([opts initial-memory] (eval-env (merge (environment initial-memory) opts))))

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
