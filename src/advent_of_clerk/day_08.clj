;; # 🎄 Advent of Code: Day 8
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (mapv (partial mapv (comp parse-long str)))))

(def ex (process "30373
25512
65332
33549
35390"))

(def data (process (slurp "input/day_08.txt")))

(defn line
  "Sequence of points, starting at (r, c) in direction (dr, dc)"
  [r c dr dc]
  (iterate (fn [[i j]]
             [(+ i dr) (+ j dc)])
           [r c]))

(line 1 3 0 -1)

(defn heights
  "Sequence of heights from tree (r, c) along direction (dr, dc)"
  [trees r c dr dc]
  (->> (line r c dr dc)
       (map (partial get-in trees))
       (take-while identity)))

(heights ex 1 3 0 -1)

(defn visible-line?
  "Is tree (r, c) visible from direction (dr, dc)"
  [trees r c dr dc]
  (let [[t & ts] (heights trees r c dr dc)]
    ;; Either it's at the edge, or it's taller than all trees in that direction:
    (or (empty? ts)
        (> t (apply max ts)))))

(visible-line? ex 1 3 0 -1)

(defn visible?
  "Is tree (r, c) visible?"
  [trees r c]
  (or (visible-line? trees r c -1 0)
      (visible-line? trees r c 1 0)
      (visible-line? trees r c 0 -1)
      (visible-line? trees r c 0 1)))

(visible? ex 1 3)

;; ## Part 1:

(defn part1 [trees]
  (->> (for [r (range (count trees))
             c (range (count (first trees)))]
         (visible? trees r c))
       (filter identity)
       (count)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn view
  "Viewing distance from tree (r, c) along direction (dr, dc)"
  [trees r c dr dc]
  (let [[t0 & ts] (heights trees r c dr dc)
        n (count (take-while #(< % t0) ts))]
    ;; If the view doesn't reach the edge, count the blocking tree too:
    (+ n (if (< n (count ts)) 1 0))))

(defn scenic
  "Scenic score of tree (r, c)"
  [trees r c]
  (* (view trees r c -1 0)
     (view trees r c 1 0)
     (view trees r c 0 -1)
     (view trees r c 0 1)))

(scenic ex 1 2)

(defn part2 [trees]
  (->> (for [r (range (count trees))
             c (range (count (first trees)))]
         (scenic trees r c))
       (apply max)))

(part2 ex)

(part2 data)
