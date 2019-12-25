(ns aoc.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go chan]]
            [clojure.pprint :refer [pprint]])
  (:refer-clojure :exclude [eval])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)))

(defn parse-intcode [s]
  (mapv #(Long/parseLong %) (str/split (str/trim s) #",")))

(defn load-param [{:keys [memory relative-base]} value param-mode]
  (condp = (or param-mode 0)
    0 (get memory value 0)                     ;position mode
    1 value                                    ;immediate mode
    2 (get memory (+ relative-base value) 0))) ;relative mode

(defn load-write-param [{:keys [ip memory relative-base param-modes]} param-index]
  (let [param-mode (get param-modes param-index 0)
        param-val (get memory (+ ip param-index 1))]
    (cond-> param-val
      (= 2 param-mode) (+ relative-base))))

(defn load-params
  [{:keys [ip memory param-modes] :as env} arity]
  (into []
    (for [i (range arity)]
      (load-param env (get memory (+ ip 1 i)) (get param-modes i 0)))))

(defn parse-op [ip memory]
  (let [op-val (get memory ip)
        digits (->> op-val str reverse (map #(Long/parseLong (str %))))
        opcode (Long/parseLong (apply str (reverse (take 2 digits))))
        param-modes (into [] (drop 2 digits))]
    {:opcode opcode
     :param-modes param-modes}))

(defn grow-memory [memory index]
  (into memory (map (constantly 0) (range (- (inc index) (count memory))))))

(defn write-memory [{:keys [memory] :as env} index value]
  (let [new-memory (assoc (cond-> memory
                            (not (contains? memory index)) (grow-memory index))
                     index value)]
    (assoc env :memory new-memory)))

(defn add-instr [{:keys [ip memory] :as env}]
  (let [[a b] (load-params env 2)
        result-index (load-write-param env 2)]
    (-> env
        (update :ip + 4)
        (write-memory result-index (+ a b)))))

(defn multiply-instr [{:keys [ip memory] :as env}]
  (let [[a b] (load-params env 2)
        result-index (load-write-param env 2)]
    (-> env
        (update :ip + 4)
        (write-memory result-index (* a b)))))

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
  [(Long/parseLong (read-line)) env])

(defmethod get-input-value :channel [{:keys [inputs input-requests] :as env}]
  (async/put! input-requests :request-input)
  [(async/<!! inputs) env])

(defn input-instr [{:keys [ip memory opcode param-modes relative-base] :as env}]
  (let [[input-value new-env] (get-input-value env)
        address (load-write-param env 0)]
    (-> new-env
        (update :ip + 2)
        (write-memory address input-value))))

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
        result-index (load-write-param env 2)]
    (-> env
        (write-memory result-index (if (< a b) 1 0))
        (update :ip + 4))))

(defn equals [{:keys [ip memory] :as env}]
  (let [[a b] (load-params env 2)
        result-index (load-write-param env 2)]
    (-> env
        (write-memory result-index (if (= a b) 1 0))
        (update :ip + 4))))

(defn adjust-relative-base [env]
  (let [[amount] (load-params env 1)]
    (-> env
        (update :relative-base + amount)
        (update :ip + 2))))

(def operations
  {1 add-instr
   2 multiply-instr
   3 input-instr
   4 output-instr
   5 jump-if-true
   6 jump-if-false
   7 less-than
   8 equals
   9 adjust-relative-base})

(defn environment [initial-memory]
  {:memory initial-memory
   :ip 0
   :relative-base 0
   :operations operations
   :input-requests (async/chan)})

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

(defn process-instruction [{:keys [operations memory ip halted? out-c] :as initial-env}]
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

(defn eval
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

(def boost-program (parse-intcode (slurp (io/resource "input/day9.txt"))))

(comment

  (run-diagnostic)

  (map :output (program-states (assoc (environment boost-program) :inputs [1])))

  )
