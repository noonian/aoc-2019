(ns aoc.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn slope [[x1 y1] [x2 y2]]
  (when-not (zero? (- x2 x1))
    (/ (- y2 y1) (- x2 x1))))

(defn in-a-line? [origin obstacle target]
  (= (slope origin obstacle)
     (slope obstacle target)))

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(defn between?
  "Return true if obstacle is exactly between origin and target."
  [origin obstacle target]
  (and (in-a-line? origin obstacle target)
       (or (< (first origin) (first obstacle) (first target))
           (> (first origin) (first obstacle) (first target))
           (= (first origin) (first obstacle) (first target)))
       (or (< (second origin) (second obstacle) (second target))
           (> (second origin) (second obstacle) (second target))
           (= (second origin) (second obstacle) (second target)))))

(defn parse-map [s]
  (let [rows (mapv #(mapv str (seq %)) (str/split-lines s))]
    rows
    (into #{}
      (for [x (range (count (first rows)))
            y (range (count rows))
            :let [v (get-in rows [y x])]
            :when (= "#" v)]
        [x y]))))

(defn asteroids-detected [asteroid-locs pos]
  (reduce (fn [detected asteroid-loc]
            (if-not
                (or (empty? asteroid-locs)
                    (some #(between? pos % asteroid-loc)
                          (disj asteroid-locs asteroid-loc pos)))
              (conj detected asteroid-loc)
              detected))
          #{}
          (disj asteroid-locs pos)))

(defn locations-by-asteroids-detected [asteroid-locs]
  (group-by (comp count (partial asteroids-detected asteroid-locs)) asteroid-locs))

(defn best-loc [asteroid-locs]
  (let [detected->locs (locations-by-asteroids-detected asteroid-locs)
        best-num (apply max (keys detected->locs))
        best-locs (get detected->locs best-num)]
    [(first best-locs) best-num]))

(defn subtract [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vaprozed-before [[x1 y1] [x2 y2]]
  (< (Math/atan2 x1 y1)
     (Math/atan2 x2 y2)))

(defn vaporization-order [asteroid-locs pos]
  (loop [locs (set asteroid-locs)
         result []]
    (if-not (seq (disj locs pos))
      result
      (let [detected (asteroids-detected locs pos)
            sorted (sort-by (fn [p]
                              (let [[x y] (subtract p pos)]
                                (- (Math/atan2 x y))))
                            detected)]
        (recur (set/difference locs detected)
               (into result sorted))))))
