;; # 🎄 Advent of Code: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [raw]
  (->> (str/split raw #"\n\n")
       (map str/split-lines)
       (map #(mapv parse-long %))))

(def data (process (slurp "input/day_01.txt")))

(def ex
  (process "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"))

;; ## Part 1:

(defn part1 [d]
  (->> d
       (map (partial apply +))
       (apply max)))

;; Example data:
(part1 ex)

;; Result:
(part1 data)

;; ## Part 2:

(defn part2 [d]
  (->> d
       (map (partial apply +))
       (sort)
       (take-last 3)
       (apply +)))

;; Example data:
(part2 ex)

;; Result:
(part2 data)
