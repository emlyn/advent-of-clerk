;; # 🎄 Advent of Code: Day 23
(ns advent-of-clerk.day-23
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (map-indexed (fn [r line]
                      (map-indexed (fn [c v]
                                     (when (= v \#)
                                       [r c]))
                                   line)))
       (mapcat identity)
       (remove nil?)
       (set)))

(def sample (process ".....
..##.
..#..
.....
..##.
....."))

(def ex (process "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."))

(def data (process (slurp "input/day_23.txt")))

(defn neighbours
  ([[r c]]
   (for [dr [-1 0 1]
         dc [-1 0 1]
         :when (not (and (zero? dr) (zero? dc)))]
     [(+ r dr) (+ c dc)]))
  ([pos dir]
   (map #(mapv + pos %)
        (case dir
          \N [[-1 -1] [-1  0] [-1  1]]
          \S [[1 -1] [1  0] [1  1]]
          \W [[-1 -1] [0 -1] [1 -1]]
          \E [[-1  1] [0  1] [1  1]]))))

(neighbours [1 2])
(neighbours [1 2] \E)

(defn move
  [pos dir]
  (mapv + pos
        (case dir
          \N [-1  0]
          \S [1  0]
          \W [0 -1]
          \E [0  1])))

(move [1 2] \E)

(defn move?
  [elves elf dir]
  (when (empty? (filter elves (neighbours elf dir)))
    (move elf dir)))

(defn proposed-move
  [elves elf dirs]
  (if (empty? (filter elves (neighbours elf)))
    elf
    (or (->> dirs
             (map (partial move? elves elf))
             (remove nil?)
             (first))
        elf)))

(defn step
  [elves dirs]
  (let [proposals (reduce #(assoc %1 %2 (proposed-move elves %2 dirs))
                          {}
                          elves)
        conflicts (->> proposals
                       vals
                       frequencies
                       (filter (comp #(> % 1) second))
                       (map first)
                       (set))]
    (reduce-kv (fn [m k v]
                 (conj m (if (conflicts v)
                           k v)))
               #{}
               proposals)))

(step sample "NSWE")

;; ## Part 1:

(let [dirs "NSWE"]
  (str (subs dirs 1) (first dirs)))

(defn run
  [elves dirs steps]
  (if (zero? steps)
    elves
    (recur (step elves dirs)
           (str (subs dirs 1) (first dirs))
           (dec steps))))

(defn area
  [elves]
  (* (->> elves
          (map first)
          (apply (juxt max min))
          (apply -)
          (inc))
     (->> elves
          (map last)
          (apply (juxt max min))
          (apply -)
          (inc))))

(area ex)

(defn part1 [d]
  (let [r (run d "NSWE" 10)]
    (- (area r) (count r))))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (loop [elves d
         dirs "NSWE"
         num 1]
    (let [next (step elves dirs)]
      (if (= next elves)
        num
        (recur next
               (str (subs dirs 1) (first dirs))
               (inc num))))))

(part2 ex)

(part2 data)
