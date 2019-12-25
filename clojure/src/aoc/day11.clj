(ns aoc.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! >!]]
            [clojure.pprint :refer [pprint]]
            [aoc.intcode :as intcode]))

(def robot-controller-program (intcode/parse-intcode (slurp (io/resource "input/day11.txt"))))

(def initial-robot-state
  {:position [0 0]
   :direction :up
   :panels {[0 0] 1}})

(defn panel-color [{:keys [position panels]}]
  (get panels position 0))

(defn paint [{:keys [position panels] :as state} color]
  #_(println (format "painting panel %s %s" (str position) (if (= color 0) "black" "white")))
  (assoc-in state [:panels position] color))

(def direction-map
  {:up {0 :left
        1 :right}
   :down {0 :right
          1 :left}
   :left {0 :down
          1 :up}
   :right {0 :up
           1 :down}})

(defn change-direction [direction left-or-right]
  (get-in direction-map [direction left-or-right]))

(defn move [{:keys [position direction] :as state}]
  (let [[x y] position]
    (assoc state :position (condp = direction
                             :up [x (inc y)]
                             :down [x (dec y)]
                             :left [(dec x) y]
                             :right [(inc x) y]))))

(defn turn [{:keys [position direction] :as state} left-or-right]
  (update state :direction change-direction left-or-right))

(defn run-robot []
  (let [state (atom initial-robot-state)
        inputs (async/chan)
        outputs (async/chan)
        state-updates (async/chan)
        {:keys [input-requests] :as initial-env}
        (assoc (intcode/environment robot-controller-program)
          :inputs inputs
          :out-c outputs)]

    (go-loop []
      (when (<! input-requests)
        (<! state-updates)
        (let [current-color (panel-color @state)]
          (>! inputs current-color)
          (recur))))

    (go-loop []
      (let [color (<! outputs)
            direction (<! outputs)]
        ;; the output channel will be closed when the program halts
        (when (and color direction)
          (try
            (swap! state (fn [{:keys [position panels] :as state}]
                           (-> state
                               (paint color)
                               (turn direction)
                               (move))))
            ;; hack to make sure the swap! happens before the input tries to read
            (async/put! state-updates true)
            (catch Exception e
              (println "caught exception")
              (.printStackTrace e)))
          (recur))))

    ;; allow initial input
    (async/put! state-updates true)

    (let [final-env (intcode/eval-env initial-env)]
      #_final-env
      state)))


;; (pprint output)
(defn print-panels [panels]
  (let [x-max (apply max (map first (keys panels)))
        y-max (apply max (map second (keys panels)))
        x-min (apply min (map first (keys panels)))
        y-min (apply min (map second (keys panels)))]
    (str/join "\n"
              (for [y (range y-max (dec y-min) -1)]
                (apply str
                       (for [x (range x-min (inc x-max))
                             :let [color (get panels [x y] 0)]]
                         (if (= color 0)
                           "."
                           "#")))))))

(comment

  ;; part 1

  (def output (run-robot))
  (count (:panels @output))

  ;; part 2
  (println (print-panels (:panels @output)))

  )
