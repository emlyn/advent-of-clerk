;; # 🎄 Advent of Code: Day 13
(ns advent-of-clerk.day-13
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

#_#_#_#_#_(declare parse-term)
(defn parse-list
  [[c & more]])
(defn patse-int
  [[c & more]])
(def digit (into #{} "0123456789"))
(defn parse-term
  [[c & more :as s]]
  (cond
    (= c \[)
    (parse-list s)

    (digit c)
    (parse-int s)))

(defn parse-input
  [s]
  (read-string s))

(defn process [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (map (partial map parse-input))))

(def ex (process "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"))

(def data (process (slurp "input/day_13.txt")))

;; ## Part 1:

(defn compare-packets
  [a b]
  (cond
    (and (int? a) (int? b))
    (when (not= a b) (- a b))

    (and (int? a) (sequential? b))
    (recur [a] b)

    (and (sequential? a) (int? b))
    (recur a [b])

    (and (sequential? a) (sequential? b))
    (or (->> (map compare-packets a b)
             (remove nil?)
             first)
        (compare-packets (count a) (count b)))))

[(compare-packets 1 2)
 (compare-packets [1] [2])
 (compare-packets [1 1] [1 2])
 (compare-packets [1] 2)
 (compare-packets [2] [1 0])]

(defn part1 [d]
  (->> d
       (map-indexed #(vector (inc %1) (apply compare-packets %2)))
       (filter #(neg? (second %)))
       (map first)
       (apply +)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (->> d
       (mapcat identity)
       (into [[[2]] [[6]]])
       (sort compare-packets)
       (map-indexed #(when (#{[[2]] [[6]]} %2) (inc %1)))
       (remove nil?)
       (apply *)))

(part2 ex)

(part2 data)
