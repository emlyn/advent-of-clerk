;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

;; Part 1
(as-> (slurp "input/day_01.txt")
      $
  (str/split $ #"\n\n")
  (map #(str/split % #"\n") $)
  (map #(map parse-int %) $)
  (map (partial apply +) $)
  (apply max $))

;; Part 2
(as-> (slurp "input/day_01.txt")
      $
  (str/split $ #"\n\n")
  (map #(str/split % #"\n") $)
  (map #(map parse-int %) $)
  (map (partial apply +) $)
  (sort $)
  (take-last 3 $)
  (apply + $))
