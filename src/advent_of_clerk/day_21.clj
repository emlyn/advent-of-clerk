;; # 🎄 Advent of Code: Day 21
(ns advent-of-clerk.day-21
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(def ops {"+" + "-" - "*" * "/" /})

(defn process [s]
  (->> s
       str/split-lines
       (map #(re-find #"([a-z]+): *(([0-9]+)|([a-z]+) *([-+*/]) *([a-z]+))" %))
       (map (fn [[_ name val _ v1 op v2]]
              [name {:val (when val (parse-long val))
                     :v1 v1
                     :v2 v2
                     :op op}]))
       (into {})))

(def ex (process "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"))

(def data (process (slurp "input/day_21.txt")))

(defn step
  [input]
  (reduce-kv (fn [m k {:keys [val op v1 v2]}]
               (if val
                 m
                 (let [val1 (get-in input [v1 :val])
                       val2 (get-in input [v2 :val])]
                   (if (and val1 val2)
                     (assoc-in m [k :val] ((ops op) val1 val2))
                     m))))
             input
             input))

(step ex)

;; ## Part 1:

(defn part1 [input]
  (loop [next input]
    (if-let [result (get-in next ["root" :val])]
      result
      (recur (step next)))))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn codestep
  [input]
  (reduce-kv (fn [m k {:keys [val op v1 v2]}]
               (if val
                 m
                 (let [val1 (get-in input [v1 :val])
                       val2 (get-in input [v2 :val])]
                   (if (and val1 val2)
                     (assoc-in m [k :val] (list (symbol op) val1 val2))
                     m))))
             input
             input))

(defn codefill
  [input]
  (if (get-in input ["root" :val])
    input
    (recur (codestep input))))

(defn makefunc [input]
  (let [incode (-> input
                   (assoc-in ["root" :op] "-")
                   (assoc-in ["humn" :val] 'val)
                   (codefill)
                   (get-in ["root" :val]))]
    (eval `(fn [~'val*] (let [~'val (bigint ~'val*)] ~incode)))))

(def func (makefunc ex))

(func 0)
(func 301)

(def func2 (makefunc data))

(defn sgn [x]
  (cond (pos? x) 1
        (neg? x) -1
        :else 0))

(defn expandrange
  ([func]
   (expandrange func (sgn (func 1)) 1 2))
  ([func sgn1 xmin xmax]
   (let [sgn2 (sgn (func xmax))]
     (cond
       (not= sgn1 sgn2)
       [xmin xmax]

       (<= xmax (long (/ Long/MAX_VALUE 2)))
       (recur func sgn2 xmax (* 2 xmax))

       :else
       (throw (Exception. "Too big"))))))

(expandrange func)

(expandrange func2)

(defn binarysearch
  ([func xmin xmax]
   (binarysearch func xmin xmax (sgn (func xmin)) (sgn (func xmax))))
  ([func xmin xmax smin smax]
   (let [xmid (bigint (/ (+ xmin xmax) 2))
         smid (sgn (func xmid))]
     (when-not (< xmin xmid xmax)
       (throw (Exception. (format "Not found: %s (%s); %s (%s); %s (%s)"
                                  xmin (func xmin)
                                  xmid (func xmid)
                                  xmax (func xmax)))))
     (cond
       (= smid smin) (recur func xmid xmax smid smax)
       (= smid smax) (recur func xmin xmid smin smid)
       :else (long xmid)))))

(binarysearch func 0 1000)

(defn part2
  [input]
  (let [f (makefunc input)
        [xmin xmax] (expandrange f)]
    (binarysearch f xmin xmax)))

(part2 ex)

(part2 data)
