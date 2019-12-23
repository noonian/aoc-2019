(ns aoc.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-digits [s]
  (mapv #(Integer/parseInt %) (map str s)))

(def input-digits (parse-digits (str/trim (slurp (io/resource "input/day8.txt")))))

(defn build-layers [width height digits]
  (let [rows (partition width digits)]
    (partition height rows)))

(defn count-occurences [digit layer]
  (get (frequencies (flatten layer)) digit 0))

(defn checksum [layers]
  (let [counted (group-by (partial count-occurences 0) layers)
        min-occurence (apply min (keys counted))
        layer (get counted min-occurence)
        freqs (frequencies (flatten layer))]
    (* (get freqs 1 0) (get freqs 2 0))))

(defn layer-pixel [& colors]
  (reduce (fn [cur pixel]
            (if (= cur 2)
              pixel ;transparent
              cur))
          2
          colors))

(defn layer-rows [& rows]
  (apply map layer-pixel rows))

(defn merge-layers [layers]
  (apply map layer-rows layers))

(defn render-image [layers]
  (let [merged (merge-layers layers)]
    (str/join "\n"
              (for [row merged]
                (str/join "" row)))))
