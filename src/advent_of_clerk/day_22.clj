;; # 🎄 Advent of Code: Day 22
(ns advent-of-clerk.day-22
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (let [[grid dirs] (str/split s #"\n\n")]
    [(str/split-lines grid)
     (->> dirs
          str/trim
          (partition-by (comp boolean (set "LR")))
          (map #(if ((set "LR") (first %))
                  (first %)
                  (parse-long (apply str %)))))]))

(def ex (process "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"))

(def data (process (slurp "input/day_22.txt")))

(defn start-pos
  [grid]
  [0
   (->> grid
        first
        (take-while (partial not= \.))
        count)])

(def directions
  {0 [0 1]
   1 [1 0]
   2 [0 -1]
   3 [-1 0]})

(defn ingrid
  [grid [r c]]
  (cond
    (neg? r) false
    (>= r (count grid)) false
    (neg? c) false
    (>= c (count (get grid r))) false
    (= \space (get-in grid [r c])) false
    :else true))

(defn move
  [grid pos dir n]
  (if (zero? n)
    pos
    (let [nextpos (mapv + pos (directions dir))
          nextpos (if (ingrid grid nextpos)
                    nextpos
                    (->> pos
                         (iterate #(mapv - % (directions dir)))
                         (take-while (partial ingrid grid))
                         (last)))]
      (if (= \. (get-in grid nextpos))
          (recur grid nextpos dir (dec n))
          pos))))

(start-pos (first ex))

(move
 (first ex)
 (start-pos (first ex))
 0
 5)

(defn step
  [grid pos dir cmd]
  (cond
    (= \L cmd)
    [pos (mod (dec dir) 4)]

    (= \R cmd)
    [pos (mod (inc dir) 4)]

    (int? cmd)
    [(move grid pos dir cmd)
     dir]))

;; ## Part 1:

(defn part1 [[grid cmds]]
  (let [[[r c] d]
        (reduce (fn [[pos dir] cmd]
                  (step grid pos dir cmd))
                [(start-pos grid) 0]
                cmds)]
    [r c d
     (+ (* 1000 (inc r))
        (* 4 (inc c))
        d)]))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (->> d))

(part2 ex)

#_(part2 data)
