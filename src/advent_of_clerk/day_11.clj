;; # 🎄 Advent of Code: Day 11
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(def ops {"+" +
          "*" *})

(defn make-op [[arg1 op arg2]]
  (let [f (ops op)]
    (fn [n]
      (f (if (= arg1 "old") n (parse-long arg1))
         (if (= arg2 "old") n (parse-long arg2))))))

((make-op ["old" "*" "19"]) 79)

(defn extract [s re]
  (if-let [m (re-find re s)]
    (rest m)
    (throw (Exception. (format "No match with '%s' for '%s'" re s)))))

(defn parse-monkey [lines]
  {:monkey (-> lines
               (nth 0)
               (extract #"Monkey ([0-9])+:")
               (first)
               (parse-long))
   :items (-> lines
              (nth 1)
              (extract #"Starting items: ([0-9, ]+)")
              (first)
              (str/split #", ?")
              (->> (mapv parse-long)))
   :op (-> lines
           (nth 2)
           (extract #"Operation: new = (old|[0-9]+) (.) (old|[0-9]+)")
           (make-op))
   :div (-> lines
            (nth 3)
            (extract #"Test: divisible by ([0-9]+)")
            (first)
            (parse-long))
   :true? (-> lines
              (nth 4)
              (extract #"If true: throw to monkey ([0-9]+)")
              (first)
              (parse-long))
   :false? (-> lines
               (nth 5)
               (extract #"If false: throw to monkey ([0-9]+)")
               (first)
               (parse-long))
   :inspected 0})

(defn process [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (map parse-monkey)
       (reduce #(assoc %1 (:monkey %2) %2)
               {})))

(def ex (process "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"))

(def data (process (slurp "input/day_11.txt")))

(defn throw-item [n monkeys item]
  (let [{:keys [op div true? false? modulo]} (monkeys n)
        worry (op item)
        worry (if modulo
                (mod worry modulo)
                (int (/ worry 3)))
        next (if (zero? (mod worry div)) true? false?)]
    (-> monkeys
        (update-in [next :items] conj worry)
        (update-in [n :inspected] inc))))

(throw-item 0 ex 79)

(defn turn [monkeys n]
  (assoc-in
   (reduce (partial throw-item n)
           monkeys
           (-> n monkeys :items))
   [n :items]
   []))

(turn ex 0)

(defn round [monkeys]
  (reduce turn
          monkeys
          (range (count monkeys))))

(round ex)

;; ## Part 1:

(defn part1 [d]
  (->> d
       (iterate round)
       (drop 20)
       (first)
       (vals)
       (map :inspected)
       (sort)
       (reverse)
       (take 2)
       (apply *)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (let [mod (->> d vals (map :div) (apply *))]
    (->> (reduce #(assoc-in %1 [%2 :modulo] mod)
                 d
                 (range (count d)))
         (iterate round)
         (drop 10000)
         (first)
         (vals)
         (map :inspected)
         (sort)
         (reverse)
         (take 2)
         (apply *))))

(part2 ex)

(part2 data)
