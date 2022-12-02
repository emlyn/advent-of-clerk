;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(def data (str/split-lines (slurp "input/day_02.txt")))

(def ex ["A Y" "B X" "C Z"])

;; ## Part 1:

(def char->num
  {\A 1 \X 1
   \B 2 \Y 2
   \C 3 \Z 3})

(defn score1 [round]
  (let [i (char->num (first round))
        j (char->num (last round))
        d (mod (- j i) 3)]
    (+ j
       ({0 3
         1 6
         2 0}
        d))))

;; Example data
(mapv score1 ex)

(apply + (map score1 data))

;; ## Part 2:

(defn score2 [round]
  (let [other (char->num (first round))
        d (dec (char->num (last round))) ;; 0=L, 1=D, 2=W
        you (+ other (dec d)) ;; 0=S, 1=R, 2=P, 3=S
        you (inc (mod (dec you) 3))] ;; 1=R, 2=P, 3=S
    (+ you (* 3 d))))

(mapv score2 ex)

(apply + (map score2 data))
