(ns aoc.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn parse-moon [s]
  (let [component-strs (-> s
                           (str/replace "<" "")
                           (str/replace ">" "")
                           (str/split #", "))
        position (for [s component-strs
                       :let [[_ value] (str/split s #"=")]]
                   (Long/parseLong value))]
    {:position (vec position)
     :velocity [0 0 0]}))

(defn parse-system [s]
  {:ts 0
   :moons (into #{} (map parse-moon (str/split-lines s)))})

(def input
  "<x=14, y=2, z=8>
<x=7, y=4, z=10>
<x=1, y=17, z=16>
<x=-4, y=-1, z=1>")

(def input-system
  {:ts 0
   :moons (into #{} (map parse-moon (str/split-lines input)))})

(defn add [v1 v2]
  (mapv + v1 v2))

(defn gravity-force* [c1 c2]
  (cond
    (= c1 c2) 0
    (> c1 c2) -1
    (< c1 c2) 1))

(defn gravity-force
  "Return vector representing the force exerted on p1 by p2. The force
  exerted on p2 by p1 will be the negation of the same vector."
  [p1 p2]
  (mapv gravity-force* p1 p2))

(defn gravitate-moons
  "Update m1 with the gravitational influence of m2."
  [m1 m2]
  (update m1 :velocity add (gravity-force (:position m1) (:position m2))))

(defn gravitate-moons-on-axis
  "Update m1 with the gravitational influence of m2."
  [axis m1 m2]
  (let [c1 (get-in m1 [:position axis])
        c2 (get-in m2 [:position axis])]
    (update-in m1 [:velocity axis] + (gravity-force* c1 c2))))

(defn update-position [{:keys [velocity] :as moon}]
  (update moon :position add velocity))

(defn apply-gravity-on-axis [{:keys [moons] :as system} axis]
  (->> (reduce (fn [updated-moons moon]
                 (conj updated-moons
                       (reduce (partial gravitate-moons-on-axis axis)
                               moon
                               (remove #(= % moon) moons))))
               []
               moons)
       (assoc system :moons)))

(defn apply-gravity [{:keys [moons] :as system}]
  (->> (reduce (fn [updated-moons moon]
                 (conj updated-moons
                       (reduce gravitate-moons moon (remove #(= % moon) moons))))
               []
               moons)
       (assoc system :moons)))

(defn update-system [system]
  (-> system
      apply-gravity
      (update :moons #(mapv update-position %))
      (update :ts inc)))

(defn update-system-on-axis [system axis]
  (-> system
      (apply-gravity-on-axis axis)
      (update :moons #(mapv update-position %))
      (update :ts inc)))

(defn potential-energy [{:keys [position] :as moon}]
  (reduce + 0 (map #(Math/abs %) position)))

(defn kinetic-energy [{:keys [velocity] :as moon}]
  (reduce + 0 (map #(Math/abs %) velocity)))

(defn total-energy [moon]
  (* (potential-energy moon)
     (kinetic-energy moon)))

(defn system-energy [{:keys [moons]}]
  (reduce + 0 (map total-energy moons)))

(defn only-component [component-index moon]
  (get-in moon [:position component-index]))

(defn steps-to-previous-state-on-axis [system axis]
  (loop [prev-states (into #{} (:moons system))
         {:keys [ts moons] :as state} system]
    (if (prev-states moons)
      (dec ts)
      (recur (conj prev-states moons) (update-system-on-axis state axis)))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b & others]
  (if others
    (apply lcm (lcm a b) others)
    (/ (* a b)
       (gcd a b))))

(defn steps-to-previous-state [system]
  (apply lcm (mapv (partial steps-to-previous-state-on-axis system) [0 1 2])))

(comment

  (pprint (take 10 (iterate #(update-system-on-axis % 0) input-system)))

  ;; part 1
  (system-energy
   (last
    (take 1001 (iterate update-system input-system))))

  ;; part 2
  (steps-to-previous-state-on-axis (parse-system "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>") 0)

  (steps-to-previous-state (parse-system "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>"))


  (lcm 18 28 44)

  (time
   (pprint (steps-to-previous-state input-system)))

  )
