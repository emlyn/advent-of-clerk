;; # 🎄 Advent of Code: Day 25
(ns advent-of-clerk.day-25
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines))

(def ex (process "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"))

(def data (process (slurp "input/day_25.txt")))

(def digits
  {\= -2
   \- -1
   \0 0
   \1 1
   \2 2})

(defn ->decimal
  [snafu]
  (reduce (fn [v d]
            (+ (* 5 v) (digits d)))
          0
          snafu))

;; 314159265
(->decimal "1121-1110-1=0")

(def sdigits
  {-2 \=
   -1 \-
   0 \0
   1 \1
   2 \2})

(defn ->snafu
  ([num]
   (->snafu num ""))
  ([num snafu]
   (if (pos? num)
     (let [d (- (mod (+ num 2) 5) 2)]
       (recur (/ (- num d) 5)
              (str (sdigits d) snafu)))
     snafu)))

(->snafu 3)
(->snafu 314159265)

;; ## Part 1:

(defn part1 [d]
  (->> d
       (map ->decimal)
       (apply +)
       (->snafu)))

(part1 ex)

(part1 data)

;; ## Part 2:
