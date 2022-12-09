;; # 🎄 Advent of Code: Day 9
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (mapv #(vector (first %) (parse-long (str (last %)))))))

(def ex (process "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"))

(def data (process (slurp "input/day_09.txt")))

(def dirs
  {\U [1 0]
   \D [-1 0]
   \L [0 -1]
   \R [0 1]})

;; ## Part 1:

(defn move [[head [tr tc]] dir]
  (let [[nhr nhc :as nh] (mapv + head (dirs dir))
        dr (- nhr tr)
        dc (- nhc tc)]
    (cond
      (< dr -1)
      (cond (< dc 0) [nh [(dec tr) (dec tc)]]
            (= dc 0) [nh [(dec tr) tc]]
            (> dc 0) [nh [(dec tr) (inc tc)]])
      (> dr 1)
      (cond (< dc 0) [nh [(inc tr) (dec tc)]]
            (= dc 0) [nh [(inc tr) tc]]
            (> dc 0) [nh [(inc tr) (inc tc)]])
      (< dc -1)
      (cond (< dr 0) [nh [(dec tr) (dec tc)]]
            (= dr 0) [nh [tr       (dec tc)]]
            (> dr 0) [nh [(inc tr) (dec tc)]])
      (> dc 1)
      (cond (< dr 0) [nh [(dec tr) (inc tc)]]
            (= dr 0) [nh [tr       (inc tc)]]
            (> dr 0) [nh [(inc tr) (inc tc)]])
      :else
      [nh [tr tc]])))

(def start [[0 0] [0 0]])

(move start \D)

(reductions move
            start
            [\D \D \R \R])

(defn part1 [d]
  (->> d
       (mapcat #(repeat (last %) (first %)))
       (reductions move start)
       (map last)
       (set)
       (count)))

(part1 ex)

(part1 data)

;; 3225: That's not the right answer; your answer is too low.

;; Debug:

(->> ex
     (mapcat #(repeat (last %) (first %)))
     (reductions move start))

;; ## Part 2:

(defn part2 [d]
  (->> d))

(part2 ex)

(part2 data)

