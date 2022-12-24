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

(defn err
  [msg & args]
  (throw (Exception. (apply format msg args))))

(defn fixup
  [[r c] dir]
  (case dir
    0 (cond
        (and (<= 0 r 49)    (= c 150))      [[(- 149 r)        99] 2] ;; 4a
        (and (<= 100 r 149) (= c 100))      [[(- 149 r)       149] 2] ;; 4b
        (and (<= 50 r 99)   (= c 100))      [[49         (+ r 50)] 3] ;; 6b
        (and (<= 150 r 199) (= c 50))       [[149       (- r 100)] 3] ;; 7b
        :else (err "Oops: [%s %s] %s" r c dir))
    1 (cond
        (and (= r 50)       (<= 100 c 149)) [[(- c 50)         99] 2] ;; 6a
        (and (= r 150)      (<= 50 c 99))   [[(+ c 100)        49] 2] ;; 7a
        (and (= r 200)      (<= 0 c 49))    [[0         (+ c 100)] 1] ;; 2b
        :else (err "Oops: [%s %s] %s" r c dir))
    2 (cond
        (and (<= 0 r 49)    (= c 49))       [[(- 149 r)         0] 0] ;; 3a
        (and (<= 50 r 99)   (= c 49))       [[100       (- r  50)] 1] ;; 5a
        (and (<= 150 r 199) (= c -1))       [[0         (- r 100)] 1] ;; 1b
        (and (<= 100 r 149) (= c -1))       [[(- 149 r)        50] 0] ;; 3b
        :else (err "Oops: [%s %s] %s" r c dir))
    3 (cond
        (and (= r -1)     (<= 50 c 99))     [[(+ c 100)         0] 0] ;; 1a
        (and (= r -1)     (<= 100 c 149))   [[199       (- c 100)] 3] ;; 2a
        (and (= r 99)     (<= 0 c 49))      [[(+ c 50)         50] 0] ;; 5b
        :else (err "Oops: [%s %s] %s" r c dir))
    (err "Oops: [%s %s] %s" r c dir)))

(defn move2
  [grid pos dir n]
  (if (zero? n)
    [pos dir]
    (let [nextpos (mapv + pos (directions dir))
          [nextpos nextdir] (if (ingrid grid nextpos)
                              [nextpos dir]
                              (fixup nextpos dir))]
      (if (= \. (get-in grid nextpos))
        (recur grid nextpos nextdir (dec n))
        [pos dir]))))

(defn step2
  [grid pos dir cmd]
  (cond
    (= \L cmd)
    [pos (mod (dec dir) 4)]

    (= \R cmd)
    [pos (mod (inc dir) 4)]

    (int? cmd)
    (move2 grid pos dir cmd)))

(defn part2 [[grid cmds]]
  (let [[[r c] d]
        (reduce (fn [[pos dir] cmd]
                  (step2 grid pos dir cmd))
                [(start-pos grid) 0]
                cmds)]
    [r c d
     (+ (* 1000 (inc r))
        (* 4 (inc c))
        d)]))

#_(part2 ex)

(part2 data)

;; 131052: That's not the right answer; your answer is too high
