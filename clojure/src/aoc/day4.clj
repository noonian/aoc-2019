(ns aoc.day4)

(defn six-digits? [n] (= 6 (count (str n))))
(defn increasing? [n]
  (every? #(apply <= %)
          (partition 2 1 (map #(Integer/parseInt (str %)) (str n)))))
(defn has-adjacent-digits? [n]
  (some #(apply = %) (partition 2 1 (str n))))

(def matches-criteria? (every-pred six-digits? increasing? has-adjacent-digits?))

(defn parse-range [s]
  (apply range (map #(Integer/parseInt %) (str/split s #"-"))))

(defn has-two-adjacent-digits? [n]
  (some #(= 2 (count %)) (partition-by identity (str n))))

(def matches-criteria-part-2? (every-pred six-digits? increasing? has-two-adjacent-digits?))
