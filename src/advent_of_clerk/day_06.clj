;; # 🎄 Advent of Code: Day 6
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  s)

(def ex1 (process "bvwbjplbgvbhsrlpgdmjqwftvncz"))
(def ex2 (process "nppdvjthqldpwncqszvftbrmjlhg"))
(def ex3 (process "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
(def ex4 (process "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(def data (process (slurp "input/day_06.txt")))

;; ## Part 1:

(defn part1 [d]
  (->> d
       (partition 4 1)
       (map set)
       (map count)
       (take-while #(< % 4))
       (count)
       (+ 4)))

(part1 ex1)

(part1 ex2)

(part1 ex3)

(part1 ex4)

(part1 data)

;; Or, as a one-liner:
(->> "input/day_06.txt" slurp (partition 4 1) (map set) (map count) (take-while #(< % 4)) count (+ 4))

;; ## Part 2:

(defn part2 [d]
  (->> d
       (partition 14 1)
       (map set)
       (map count)
       (take-while #(< % 14))
       (count)
       (+ 14)))

(part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(part2 ex1)

(part2 ex2)

(part2 ex3)

(part2 ex4)

(part2 data)

