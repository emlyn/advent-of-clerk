;; # 🎄 Advent of Code: Day 14
(ns advent-of-clerk.day-14
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn process [s]
  (->> s
       str/split-lines
       (map #(str/split % #" -> "))
       (map (partial map #(str/split % #",")))
       (map (partial map (partial mapv parse-long)))))

(def ex (process "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"))

(def data (process (slurp "input/day_14.txt")))

(def initial-pos [0 500])

(defn draw-vert
  [{:keys [minpos] :as board} c startr endr]
  (let [[minr minc] minpos]
    (reduce #(assoc-in %1 [:grid (- %2 minr) (- c minc)] \#)
             board
             (range (min startr endr) (inc (max startr endr))))))

(defn draw-horiz
  [{:keys [minpos] :as board} startc endc r]
  (let [[minr minc] minpos]
    (reduce #(assoc-in %1 [:grid (- r minr) (- %2 minc)] \#)
            board
            (range (min startc endc) (inc (max startc endc))))))

(defn draw-segment
  [board [[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (draw-vert board x1 y1 y2)
    (= y1 y2) (draw-horiz board x1 x2 y1)
    :else (throw (Exception. "diagonal line segment"))))

(defn draw-line
  [board line]
  (reduce draw-segment
          board
          (partition 2 1 line)))

(defn make-board
  [lines floor?]
  (let [rs (mapcat (partial map last) lines)
        cs (mapcat (partial map first) lines)
        minr (apply min (first initial-pos) rs)
        maxr (+ (apply max (first initial-pos) rs)
                (if floor? 2 0))
        minc (- (apply min (last initial-pos) cs)
                (if floor? (- maxr minr) 0))
        maxc (+ (apply max (last initial-pos) cs)
                (if floor? (- maxr minr) 0))]
    (reduce draw-line
            {:running true
             :num 0
             :minpos [minr minc]
             :maxpos [maxr maxc]
             :pos initial-pos
             :grid (vec (repeat (inc (- maxr minr))
                                (vec (repeat (inc (- maxc minc)) \.))))}
            (into (if floor? [[[minc maxr] [maxc maxr]]] [])
                  lines))))

(defn draw-board
  [{:keys [grid minpos pos]}]
  (-> grid
       (assoc-in (mapv - pos minpos) \+)
       (->> (map (partial apply str))
            (str/join \newline))))

(make-board ex false)

(draw-board
 (make-board ex false))

(defn valid?
  [{:keys [minpos maxpos]} [r c]]
  (let [[minr minc] minpos
        [maxr maxc] maxpos]
    (and (<= minr r maxr)
         (<= minc c maxc))))

(defn possible?
  [{:keys [minpos grid]} pos]
  (= \. (get-in grid (mapv - pos minpos))))

(defn next-pos
  [[r c]]
  [[(inc r) c] [(inc r) (dec c)] [(inc r) (inc c)]])

(defn step
  [{:keys [minpos pos] :as board}]
  (loop [[next & more] (next-pos pos)]
    (cond
      (nil? next)
      (-> board
          (update :num inc)
          (assoc-in (into [:grid] (mapv - pos minpos)) \o)
          (assoc :pos initial-pos)
          (assoc :running (not= pos initial-pos)))

      (not (valid? board next))
      (assoc board :running false)

      (possible? board next)
      (assoc board :pos next)

      :else
      (recur more))))

(defn simulate
  [data & [floor?]]
  (loop [board (make-board data floor?)]
    (if (:running board)
      (recur (step board))
      board)))

;; ## Part 1:

(def final1-ex
  (simulate ex))

(draw-board final1-ex)

(:num final1-ex)

(def final1
  (simulate data))

(:num final1)

;; ## Part 2:

(def final2-ex
  (simulate ex true))

(draw-board final2-ex)

(:num final2-ex)

(def final2
  (simulate data true))

(:num final2)
