;; # 🎄 Advent of Code: Day 10
(ns advent-of-clerk.day-10
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (map #(str/split % #" "))
       (map #(if (<= (count %) 1)
               % [(first %) (parse-long (last %))]))))

(def ex (process "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"))

(def data (process (slurp "input/day_10.txt")))

(defn execute
  ([prog]
   (execute prog [[1 1]]))
  ([[[cmd arg] & rest] result]
   (let [[t v] (last result)]
     (case cmd
       nil result
       "noop" (recur rest (conj result [(inc t) v]))
       "addx" (recur rest (conj result [(inc t) v] [(+ t 2) (+ v arg)]))))))

;; ## Part 1:

(defn part1 [d]
  (->> d
       execute
       (filter #(-> % first (+ 20) (mod 40) zero?))
       (map (partial apply *))
       (take 6)
       (apply +)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (->> (map -
            (cycle (range 40))
            (->> d execute (map second)))
       (map #(-> % abs {0 \# 1 \#} (or \.)))
       (partition 40)
       (map (partial apply str))
       (str/join \newline)))

(part2 ex)

(part2 data)
