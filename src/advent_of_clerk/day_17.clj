;; # 🎄 Advent of Code: Day 17
(ns advent-of-clerk.day-17
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn parse-rock
  [d]
  (->> d
       (reverse)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x v]
                                     (when (= v \#)
                                       [x y]))
                                   line)))
       (mapcat identity)
       (remove nil?)))

(def rocks
  (->> (str/split "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##" #"\n\n")
       (map str/split-lines)
       (map parse-rock)))

(def ex ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(count ex)

(def data (str/trim (slurp "input/day_17.txt")))

(count data)

(defn top
  [grid]
  (->> grid
       (map second)
       (apply max -1)))

(defn push
  [grid rock dir]
  (let [nextrock (map (partial mapv + [(case dir \< -1 \> 1) 0])
                      rock)
        [minx maxx] (->> nextrock (map first) (apply (juxt min max)))]
    (if (or (< minx 0)
            (> maxx 6)
            (some grid nextrock))
      rock
      nextrock)))

(push nil [[0 0] [1 3] [4 2]] \<)

(defn down
  [grid rock]
  (let [nextrock (map (partial mapv + [0 -1])
                      rock)]
    (when-not (some grid nextrock)
      nextrock)))

(defn start-rock
  [grid rock]
  (map (partial mapv + [2 (+ 4 (top grid))])
       rock))

(defn drop-rock
  [grid gas rock]
  (loop [[nextgas & moregas] gas
         rock (start-rock grid rock)]
    (let [rock (push grid rock nextgas)]
      (if-let [next-rock (down grid rock)]
        (recur moregas next-rock)
        [(into grid rock) moregas]))))

(defn show
  [grid]
  (str
   (str/join \newline
             (for [y (reverse (range (inc (top grid))))]
               (apply str (concat [\|] (map #(if (grid [% y]) \# \.) (range 7)) [\|]))))
   "\n+-------+"))

(def startgrid (into #{} (map #(vector % -1) (range 7))))

(def rock1 (drop-rock startgrid ex (first rocks)))

(show (first rock1))

;; ## Part 1:

(defn part1 [d num]
  (loop [grid startgrid
         [rock & morerock] (cycle rocks)
         gas (cycle d)
         num num]
    (if (pos? num)
      (let [[grid gas] (drop-rock grid gas rock)]
        (recur grid morerock gas (dec num)))
      (inc (top grid)))))

(part1 ex 2022)

(part1 data 2022)

;; ## Part 2:
