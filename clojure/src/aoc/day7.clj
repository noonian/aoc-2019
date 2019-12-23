(ns aoc.day7
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [aoc.intcode :as intcode]
            [clojure.core.async :as async :refer [go chan]]
            [clojure.pprint :refer [pprint]]))

(def amp-controller-software (intcode/parse-intcode (slurp (io/resource "input/day7.txt"))))
(def p1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")

(defn run-amp [input phase-setting]
  (try
    (:output
     (intcode/eval {:inputs [phase-setting input]} #_p1 amp-controller-software))
    (catch Exception e
      (.printStackTrace e))))

(defn run-amp-env [env]
  (async/thread (intcode/eval-env env)))

(defn run-amps-sync [phase-settings]
  (reduce run-amp 0 phase-settings))

(defn run-amps [phase-settings]
  (let [amp-envs
        (into []
          (for [[i phase-setting] (map-indexed vector phase-settings)
                :let [input-c (async/chan)
                      output-c (async/chan)]]
            (do (async/put! input-c phase-setting)
                (assoc (intcode/environment amp-controller-software)
                  :inputs input-c
                  :out-c output-c))))]
    (async/put! (:inputs (first amp-envs)) 0)
    (doseq [i (range (count phase-settings))
            :let [out-c (:out-c (get amp-envs i))
                  input-c (:inputs (get amp-envs (mod (inc i) 5)))]]
      (async/pipe out-c input-c))
    (:output (last (mapv #(async/<!! %)
                         (map run-amp-env amp-envs))))))

;; part 1

(defn max-thruster-signal []
  (apply max (map run-amps-sync (combo/permutations (range 5)))))

(defn max-thruster-signal-feedback []
  (apply max (map run-amps (combo/permutations (range 5 10)))))

;; The examples seem to be wrong

#_(run-amps [4 3 2 1 0])
;; (run-amps [3 1 2 4 0])
;; (run-amps [0 1 2 3 4])
;; (run-amps [1 0 4 3 2])

;; (def states
;;   (intcode/program-states (assoc (intcode/environment amp-controller-software)
;;                             :inputs [0 4388])))

;; (pprint (map :output states))
;; (count states)



;; 10 ->
;; 48 ->
;; 438 ->
;; 4388 -> 4393
