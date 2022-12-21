;; # 🎄 Advent of Code: Day 20
(ns advent-of-clerk.day-20
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (mapv parse-long)))

(def ex (process "1
2
-3
3
-2
0
4"))

(def data (process (slurp "input/day_20.txt")))

(defn indexed
  [data]
  (vec (map-indexed vector data)))

(indexed ex)

(defn mix-nth
  [idata n]
  (let [[pos item] (->> idata
                        (map-indexed (fn [pos [i v]]
                                       [pos [i v] (= i n)]))
                        (drop-while (comp false? last))
                        (first))
        temp-data (into (subvec idata 0 pos) (subvec idata (inc pos)))
        new-pos (inc (mod (dec (+ pos (second item))) (count temp-data)))]
    (into (conj (subvec temp-data 0 new-pos) item)
          (subvec temp-data new-pos))))

(mix-nth (indexed ex) 2)

(defn mix-round
  [d]
  (reduce mix-nth
          d
          (range (count d))))

(->> ex indexed mix-round (mapv last))

;; ## Part 1:

(defn part1 [d]
  (let [mixed (->> d indexed mix-round (mapv last))
        zpos
        (->> mixed
             (map-indexed (fn [_ v] (zero? v)))
             (take-while false?)
             (count))]
    (apply +
           (map #(get mixed (mod (+ zpos %) (count d)))
                [1000 2000 3000]))))

(part1 ex)

(part1 data)

;; ## Part 2:

(->> ex
     (map (partial * 811589153))
     (indexed)
     (iterate mix-round)
     (drop 10)
     (first)
     (mapv last))

(defn part2 [d]
  (let [mixed (->> d
                   (map (partial * 811589153))
                   (indexed)
                   (iterate mix-round)
                   (drop 10)
                   (first)
                   (mapv last))
        zpos (->> mixed
                  (map-indexed (fn [_ v] (zero? v)))
                  (take-while false?)
                  (count))]
    (apply +
           (map #(get mixed (mod (+ zpos %) (count d)))
                [1000 2000 3000]))))

(part2 ex)

(part2 data)
