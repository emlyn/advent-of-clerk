;; # 🎄 Advent of Code: Day 24
(ns advent-of-clerk.day-24
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines))

(def ex (process "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"))

(def data (process (slurp "input/day_24.txt")))

(defn start-pos
  [grid]
  [0 (->> grid
          first
          (reduce #(if (= %2 \.) (reduced %1) (inc %1)) 0))])

(defn end-pos
  [grid]
  [(dec (count grid))
   (->> grid
        last
        (reduce #(if (= %2 \.) (reduced %1) (inc %1)) 0))])

(start-pos ex)
(end-pos ex)
(start-pos data)

(defn blizzards
  [d]
  (->> d
       (map-indexed (fn [r line]
                      (map-indexed (fn [c v]
                                     (case v
                                       \^ [[r c] [-1 0]]
                                       \v [[r c] [1 0]]
                                       \< [[r c] [0 -1]]
                                       \> [[r c] [0 1]]
                                       nil))
                                   line)))
       (mapcat identity)
       (remove nil?)))

(defn walls
  [d]
  (->> d
       (map-indexed (fn [r line]
                      (map-indexed (fn [c v]
                                     (if (= v \#) [r c] nil))
                                   line)))
       (mapcat identity)
       (remove nil?)
       (into #{})))

(blizzards ex)
(walls ex)

(defn neighbours
  [rlimit [r c]]
  (remove nil?
          [[r c] [r (inc c)] [r (dec c)]
           (when (< r rlimit)
             [(inc r) c])
           (when (pos? r)
             [(dec r) c])]))

(defn move-blizzard
  [walls pos dir]
  (let [next (mapv + pos dir)]
    (if-not (walls next)
      next
      (->> pos
           (iterate #(mapv - % dir))
           (take-while (comp not walls))
           (last)))))

(defn step
  [walls blizzards pos rlimit]
  (let [nextblizzards (map (fn [[pos dir]]
                             [(move-blizzard walls pos dir) dir])
                           blizzards)
        blocked (into walls (map first nextblizzards))
        nextpos (->> pos
                     (mapcat (partial neighbours rlimit))
                     (remove blocked)
                     (set))]
    [nextblizzards nextpos]))

(step (walls ex)
      (blizzards ex)
      [(start-pos ex)]
      (dec (count ex)))

;; ## Part 1:

(defn part1 [d]
  (let [end (end-pos d)
        walls (walls d)]
    (loop [bliz (blizzards d)
           pos #{(start-pos d)}
           n 0]
      (if (pos end)
        n
        (let [[nbliz npos] (step walls bliz pos (dec (count d)))]
          (recur nbliz npos (inc n)))))))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (let [start (start-pos d)
        end (end-pos d)
        walls (walls d)
        rlimit (dec (count d))]
    (loop [bliz (blizzards d)
           pos #{start}
           n 0
           leg 0]
      (case leg
        0 (if (pos end)
            (recur bliz #{end} n (inc leg))
            (let [[nbliz npos] (step walls bliz pos rlimit)]
              (recur nbliz npos (inc n) leg)))
        1 (if (pos start)
            (recur bliz #{start} n (inc leg))
            (let [[nbliz npos] (step walls bliz pos rlimit)]
              (recur nbliz npos (inc n) leg)))
        2 (if (pos end)
            n
            (let [[nbliz npos] (step walls bliz pos rlimit)]
              (recur nbliz npos (inc n) leg)))))))

(part2 ex)

(part2 data)
