;; # 🎄 Advent of Code: Day 18
(ns advent-of-clerk.day-18
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (map #(str/split % #","))
       (map (partial mapv parse-long))
       (set)))

(def ex (process "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"))

(def data (process (slurp "input/day_18.txt")))

;; ## Part 1:

(defn neighbours
  [[x y z]]
  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

(neighbours [1 2 3])

(defn part1 [d]
  (let [[seen ntouch]
        (reduce (fn [[seen ntouch] block]
                  [(conj seen block) (+ ntouch (count (filter seen (neighbours block))))])
                [#{} 0]
                d)]
    (- (* (count seen) 6)
       (* ntouch 2))))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn extent
  [d]
  (map #(->> d
             (map %)
             (apply (juxt min max)))
       [first second last]))

(extent ex)

(defn part2 [d]
  (let [[[x1 x2] [y1 y2] [z1 z2]] (extent d)
        outer (into {}
                    (for [x (range x1 (inc x2))
                          y (range y1 (inc y2))
                          z (range z1 (inc z2))
                          :when (not (d [x y z]))]
                      [[x y z] (boolean (or (#{x1 x2} x)
                                            (#{y1 y2} y)
                                            (#{z1 z2} z)))]))]
    (loop [outer outer]
      (let [next (reduce-kv (fn [m k v]
                              (assoc m k (or v (some outer (neighbours k)))))
                            {}
                            outer)]
        (if (= next outer)
          (->> next
               (remove second)
               (keys)
               (part1)
               (- (part1 d)))
          (recur next))))))

(part2 ex)

(part2 data)
