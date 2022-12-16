;; # 🎄 Advent of Code: Day 15
(ns advent-of-clerk.day-15
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (map (partial re-find #"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)"))
       (map rest)
       (map (partial map parse-long))
       (map (partial split-at 2))
       (into {})))

(def ex (process "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"))

(def data (process (slurp "input/day_15.txt")))

(defn minmax
  [dim d]
  (->> d
       (mapcat identity)
       (map dim)
       (apply (juxt min max))))

(minmax first ex)

;; ## Part 1:

(defn manhattan
  [pos1 pos2]
  (->> [pos1 pos2]
       (apply map -)
       (map abs)
       (apply +)))

(manhattan [1 2] [3 4])

(defn intersect
  [[sx sy] dist yval]
  (let [halflen (- dist (abs (- sy yval)))]
    (range (- sx halflen) (inc (+ sx halflen)))))

(intersect [10 12] 4 10)

(defn part1 [sensors yval]
  (let [maxdist (->> sensors
                     (map (partial apply manhattan))
                     (apply max))
        line (->> sensors
                  (minmax first)
                  (map + [(- maxdist) (inc maxdist)])
                  (apply range)
                  (reduce #(assoc %1 %2 false)
                          {}))
        beacons (->> sensors
                     (vals)
                     (filter #(= yval (second %)))
                     (map first)
                     (into #{}))]
    (->> sensors
         (reduce (fn [line [sensor beacon]]
                   (let [dist (manhattan sensor beacon)]
                     (reduce #(assoc %1 %2 true)
                             line
                             (intersect sensor dist yval))))
                 line)
         (filter #(and (second %) (not (beacons (first %)))))
         (count))))

(part1 ex 10)

(part1 data 2000000)

;; 3911634 : That's not the right answer; your answer is too low.

;; 5733733 : That's not the right answer; your answer is too high.

;; ## Part 2:

(defn part2 [d]
  (->> d))

(part2 ex)

(part2 data)
