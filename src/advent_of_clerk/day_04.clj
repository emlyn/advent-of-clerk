;; # 🎄 Advent of Code: Day 4
(ns advent-of-clerk.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       (str/split-lines)
       (map #(str/split % #","))
       (map (partial map #(str/split % #"-")))
       (map (partial mapv (partial mapv parse-long)))))

(def ex (process "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"))

(def data (process (slurp "input/day_04.txt")))

;; ## Part 1:

(defn part1 [d]
  (->> d
       (filter (fn [[[min1 max1] [min2 max2]]]
                 (or (<= min1 min2 max2 max1)
                     (<= min2 min1 max1 max2))))
       (count)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (->> d
       (filter (fn [[[min1 max1] [min2 max2]]]
                 (or (<= min1 min2 max1)
                     (<= min1 max2 max1))))
       (count)))

(part2 ex)

(part2 data)
