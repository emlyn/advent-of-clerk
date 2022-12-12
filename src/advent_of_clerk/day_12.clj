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

(defn neighbours
  "Neighbouring cells of cell (r, c)"
  [[r c] grid]
  (cond-> []
    (pos? r) (conj [(dec r) c])
    (pos? c) (conj [r (dec c)])
    (< r (dec (count grid))) (conj [(inc r) c])
    (< c (dec (count (first grid)))) (conj [r (inc c)])))

(neighbours [0 1] ex)

(defn valid-move?
  "Is it possible to move from cell `from` to cell `to` given the height map"
  [heights from to]
  (>= (->> from (get-in heights) to-height)
      (->> to (get-in heights) to-height dec)))

(valid-move? ex [2 0] [2 1])
(valid-move? ex [2 0] [3 0])

(defn valid-neighbours
  [heights pos]
  (->> heights
       (neighbours pos)
       (filter (partial valid-move? heights pos))))

(defn positions
  "Positions where `chr` appears in the grid"
  [chr grid]
  (->> grid
       (mapv #(str/index-of % chr))
       (map vector
            (range))
       (filter second)))

(defn position
  "Position of first `chr` in the grid"
  [chr grid]
  (first (positions chr grid)))

(position \E ex)

(defn distance
  ([heights start end]
   (distance heights end [start] #{} 0))
  ([heights end positions seen n]
   (if (some (partial = end) positions)
     n
     (when-let [next (->> positions
                          (mapcat (partial valid-neighbours heights))
                          (set)
                          (remove seen)
                          (seq))]
       (recur heights end next (into seen positions) (inc n))))))

(distance ex [0 0] [0 1])

;; ## Part 1:

(defn part1 [heights]
  (distance heights
            (position \S heights)
            (position \E heights)))

(part1 ex)

(part1 data)

;; ## Part 2:

(positions \a ex)

(defn part2 [heights]
  (let [starts (into [(position \S heights)]
                     (positions \a heights))
        end (position \E heights)]
    (->> starts
         (map #(distance heights % end))
         (remove nil?)
         (apply min))))

(part2 ex)

(part2 data)
