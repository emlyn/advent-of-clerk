;; # 🎄 Advent of Code: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:
(def data
  (->> (str/split (slurp "input/day_01.txt") #"\n\n")
       (map str/split-lines)
       (map #(mapv parse-long %))))

;; ## Part 1:
(->> data
     (map (partial apply +))
     (apply max))

;; ## Part 2:
(->> data
     (map (partial apply +))
     (sort)
     (take-last 3)
     (apply +))
