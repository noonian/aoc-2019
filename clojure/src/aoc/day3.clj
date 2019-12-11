(ns aoc.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn parse-edge
  "Return all points in an edge of a wire beginning at pos."
  [pos s]
  (let [[x-start y-start] pos
        [_ direction distance-str] (re-matches #"([LRUD])([0-9]+)" s)
        distance (Integer/parseInt distance-str)]
    (condp = direction
      "L" (for [x (range x-start (- x-start distance 1) -1)] [x y-start])
      "R" (for [x (range x-start (+ x-start distance 1))] [x y-start])
      "U" (for [y (range y-start (+ y-start distance 1))] [x-start y])
      "D" (for [y (range y-start (- y-start distance 1) -1)] [x-start y]))))

(defn parse-wire [s]
  (reduce (fn [{:keys [pos] :as wire} s]
            (let [edge (parse-edge pos s)]
              (-> wire
                  (update :edges conj edge)
                  (update :points set/union (set edge))
                  (assoc :pos (last edge)))))
          {:edges []
           :points #{}
           :pos [0 0]}
          (str/split s #",")))

(defn join-edge [e1 e2]
  (into [] (concat (butlast e1) e2)))

(defn steps [w1 intersection]
  (loop [[pos & others :as points] (reduce join-edge [] (:edges w1))
         count 0]
    (when (seq points)
      (if (= pos intersection)
        count
        (recur others (inc count))))))

(defn intersection-points [w1 w2]
  (set/intersection (:points w1) (:points w2)))

(defn solve [w1 w2]
  (->> (disj (intersection-points w1 w2) [0 0])
       (map (partial manhattan-distance [0 0]))
       (apply min)))

(defn fewest-combined-steps [w1 w2]
  (->> (disj (intersection-points w1 w2) [0 0])
       (map (fn [intersection] (+ (steps w1 intersection)
                                  (steps w2 intersection))))
       (apply min)))
