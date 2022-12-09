;; # 🎄 Advent of Code: Day 9
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (map #(str/split % #" "))
       (map #(vector (ffirst %) (parse-long (last %))))))

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

(defn update-tail [[hr hc] [tr tc]]
  (let [dr (- hr tr)
        dc (- hc tc)]
    (cond
      (< dr -1)
      (cond (< dc 0) [(dec tr) (dec tc)]
            (= dc 0) [(dec tr) tc]
            (> dc 0) [(dec tr) (inc tc)])
      (> dr 1)
      (cond (< dc 0) [(inc tr) (dec tc)]
            (= dc 0) [(inc tr) tc]
            (> dc 0) [(inc tr) (inc tc)])
      (< dc -1)
      (cond (< dr 0) [(dec tr) (dec tc)]
            (= dr 0) [tr       (dec tc)]
            (> dr 0) [(inc tr) (dec tc)])
      (> dc 1)
      (cond (< dr 0) [(dec tr) (inc tc)]
            (= dr 0) [tr       (inc tc)]
            (> dr 0) [(inc tr) (inc tc)])
      :else
      [tr tc])))

;; ## Part 1:

(defn move [[head tail] dir]
  (let [new-head (mapv + head (dirs dir))]
    [new-head (update-tail new-head tail)]))

(def start [[0 0] [0 0]])

(move start \R)

(reductions move
            start
            [\U \U \R \R])

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

(defn move2 [[head & rest] dir]
  (let [new-head (mapv + head (dirs dir))]
    (reductions update-tail new-head rest)))

(def start2 (vec (repeat 10 [0 0])))

(move2 start2 \R)

(reductions move2
            start2
            [\U \U \R \R])

(defn part2 [d]
  (->> d
       (mapcat #(repeat (last %) (first %)))
       (reductions move2 start2)
       (map last)
       (set)
       (count)))

(part2 ex)

(part2 data)

