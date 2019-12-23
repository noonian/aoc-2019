(ns aoc.day6
  (:require [clojure.string :as str]))

(defn parse-direct-orbit [s]
  (let [[orbited orbiting] (str/split (str/trim s) #"\)")]
    [orbiting orbited]))

(defn parse-orbits [s]
  (into {} (map parse-direct-orbit (str/split-lines s))))

(defn orbits* [direct-orbits label]
  (loop [label label
         result []]
    (if-let [direct-orbit (get direct-orbits label)]
      (recur direct-orbit (conj result direct-orbit))
      result)))

(def orbits (memoize orbits*))

(defn count-total-orbits [direct-orbits]
  (let [labels (keys direct-orbits)]
    (reduce + 0 (map (comp count (partial orbits direct-orbits)) labels))))

;; find common ancestor, add lengths of boths orbits minus the shared root path

(defn closest-common-ancestor [a-orbits b-orbits]
  (loop [result (first a-orbits)
         a (reverse a-orbits)
         b (reverse b-orbits)]
    (if (= (first a) (first b))
      (recur (first a) (rest a) (rest b))
      result)))

(defn relevant-subbranches [a-orbits b-orbits]
  (let [cca (closest-common-ancestor a-orbits b-orbits)]
    [(drop-while (partial not= cca) (reverse a-orbits))
     (drop-while (partial not= cca) (reverse b-orbits))]))

(defn min-orbital-transfers [direct-orbits a b]
  (-
   (reduce + 0
           (map count
                (relevant-subbranches (orbits direct-orbits a)
                                      (orbits direct-orbits b))))
   2))
