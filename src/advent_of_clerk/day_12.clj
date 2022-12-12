;; # 🎄 Advent of Code: Day 12
(ns advent-of-clerk.day-12
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines))

(def ex (process "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"))

(def data (process (slurp "input/day_12.txt")))

(def to-height
  (into {\S 1 \E 26}
        (for [i (range 26)]
          [(char (+ (int \a) i))
           (inc i)])))

;; ## Part 1:

(defn neighbours [[r c] grid]
  (cond-> []
    (pos? r) (conj [(dec r) c])
    (pos? c) (conj [r (dec c)])
    (< r (dec (count grid))) (conj [(inc r) c])
    (< c (dec (count (first grid)))) (conj [r (inc c)])))

(neighbours [0 1] ex)

(defn valid-move? [heights from to]
  (>= (->> from (get-in heights) to-height)
      (->> to (get-in heights) to-height dec)))

(valid-move? ex [2 0] [2 1])
(valid-move? ex [2 0] [3 0])

(defn update-grid [[heights grid] pos]
  (if-let [nearest
           (->> grid
                (neighbours pos)
                (filter #(valid-move? heights % pos))
                (map (partial get-in grid))
                (remove nil?)
                (seq))]
    [heights (assoc-in grid pos (inc (apply min nearest)))]
    [heights grid]))

(defn start-grid [heights start]
  (assoc-in (vec (repeat (count heights)
                         (vec (repeat (count (first heights)) nil))))
            start
            0))

#_(start-grid ex [0 0])


(update-grid [ex (start-grid ex [0 0])] [0 1])
(update-grid [ex (start-grid ex [0 0])] [0 2])

(defn step [heights grid]
  (let [result
        (reduce update-grid
                [heights grid]
                (for [r (range (count grid))
                      c (range (count (first grid)))
                      :when (nil? (get-in grid [r c]))]
                  [r c]))]
    (when (not= (last result) grid)
      result)))

(step ex (start-grid ex [0 0]))

(defn positions [chr grid]
  (->> grid
       (mapv #(str/index-of % chr))
       (map vector
            (range))
       (filter second)))

(defn position [chr grid]
  (first (positions chr grid)))

(position \E ex)

(defn distance [heights start end]
  (loop [grid (start-grid heights start)]
    (if-let [n (get-in grid end)]
      n
      (when-let [next (last (step heights grid))]
        (recur next)))))

(defn part1 [heights]
  (distance heights
            (position \S heights)
            (position \E heights)))

(part1 ex)

(part1 data)

;; ## Part 2:

(positions \a ex)

(defn part2 [heights]
  (let [starts
        (into [(position \S heights)]
              (positions \a heights))
        end (position \E heights)]
    (->> starts
         (map #(distance heights % end))
         (remove nil?)
         (apply min))))

(part2 ex)

(part2 data)
