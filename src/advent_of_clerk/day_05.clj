;; # 🎄 Advent of Code: Day 5
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn assert-sorted [coll]
  (assert (= coll (sort coll))
          (format "Collection is not sorted: %s" coll))
  coll)

(defn parse-stack [s]
  (->> s
       str/split-lines
       (map #(take-nth 4 (rest %)))
       reverse
       (apply map vector)
       (assert-sorted)
       (map #(vector (first %) (vec (take-while (partial not= \space) (rest %)))))
       (into {})))

(defn parse-moves [s]
  (->> s
       str/split-lines
       (map #(re-find #"move ([0-9]+) from (\w) to (\w)" %))
       (map (fn [[_ n s d]]
              [(parse-long n) (first s) (first d)]))))

(defn top-crates [stack]
  (->> stack
       keys
       sort
       (map stack)
       (map last)
       (apply str)))

(defn process [s]
  (let [[stack moves] (str/split s #"\n\n")]
    [(parse-stack stack)
     (parse-moves moves)]))

(def ex (process "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"))

(def data (process (slurp "input/day_05.txt")))

;; ## Part 1:

(defn do-move1 [stack [n src dest]]
  (assert (stack src))
  (assert (stack dest))
  (assert (>= (count (stack src)) n)
          (format "Error: %s %s %s %s" stack n src dest))
  (if (pos? n)
    (recur (-> stack
               (update dest conj (last (stack src)))
               (update src (comp vec butlast)))
           [(dec n) src dest])
    stack))

(defn part1 [[stack moves]]
  (top-crates
   (reduce do-move1 stack moves)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn do-move2 [stack [n src dest]]
  (assert (stack src))
  (assert (stack dest))
  (assert (>= (count (stack src)) n)
          (format "Error: %s %s %s %s" stack n src dest))
  (let [[remain taken] (split-at (- (count (stack src)) n)
                                 (stack src))]
    (-> stack
        (update dest into taken)
        (assoc src (vec remain)))))

(defn part2 [[stack moves]]
  (top-crates
   (reduce do-move2 stack moves)))

(part2 ex)

(part2 data)
