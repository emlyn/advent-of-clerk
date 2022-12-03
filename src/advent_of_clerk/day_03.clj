;; # 🎄 Advent of Code: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.set :as set]))

(def ex "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def data (slurp "input/day_03.txt"))

(def priority
  (into {}
        (map vector
             "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
             (next (range)))))

;; ## Part 1:

(defn total-priority [d]
  (->> d
       str/split-lines
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
       str/split-lines
       (map set)
       (partition 3)
       (map (partial reduce set/intersection))
       (map first)
       (map priority)
       (apply +)))

(badges ex)

(badges data)
