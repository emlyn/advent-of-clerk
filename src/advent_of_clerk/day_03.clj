;; # 🎄 Advent of Code: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn process [s]
  (str/split-lines s))

(def ex (process "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"))

(def data (process (slurp "input/day_03.txt")))

(def priority
  (into {}
        (map #(vector (char %1) (inc %2))
             (concat (range (int \a) (inc (int \z)))
                     (range (int \A) (inc (int \Z))))
             (range))))

;; ## Part 1:

(defn total-priority [d]
  (->> d
       (map #(partition (/ (count %) 2) %))
       (map #(set/intersection (set (first %)) (set (last %))))
       (map first)
       (map priority)
       (apply +)))

(total-priority ex)

(total-priority data)

;; ## Part 2:

(defn badges [d]
  (->> d
       (map set)
       (partition 3)
       (map (partial reduce set/intersection))
       (map first)
       (map priority)
       (apply +)))

(badges ex)

(badges data)
