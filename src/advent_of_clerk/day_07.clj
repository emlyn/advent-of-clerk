;; # 🎄 Advent of Code: Day 7
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; ## Common:

(defn cd [pwd dir]
  (case dir
    "/"  []
    ".." (vec (butlast pwd))
    (conj pwd dir)))

(defn process [s]
  (loop [pwd []
         tree {}
         ls? false
         [line & rest] (str/split-lines s)]
    (cond
      (or (not line) (empty? line))
      tree

      (and ls? (str/starts-with? line "$ "))
      (recur pwd
             tree
             false
             (cons line rest))

      (and ls? (str/starts-with? line "dir "))
      (let [[_ fname] (str/split line #" ")]
        (recur pwd
               (assoc-in tree (conj pwd fname) {})
        ls?
        rest))

      ls?
      (let [[size fname] (str/split line #" ")
            size (parse-long size)]
        (recur pwd
               (assoc-in tree (conj pwd fname) size)
               ls?
               rest))

      (str/starts-with? line "$ cd ")
      (recur (cd pwd (subs line 5))
             tree
             false
             rest)

      (str/starts-with? line "$ ls")
      (recur pwd tree true rest)

      :else
      (throw (Exception. (format "oops: %s" line))))))

(def ex (process "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"))

(def data (process (slurp "input/day_07.txt")))

(defn treesize [tree]
  (cond
    (map? tree)
    (apply + (map treesize (vals tree)))

    (int? tree)
    tree

    :else
    (throw (Exception. (format "oops: %s" tree)))))

(defn dirs
  ([tree]
   (dirs tree []))
  ([ tree path]
   (let [subtree (get-in tree path)]
     (if (map? subtree)
       (lazy-seq (cons path
                       (mapcat (fn [d]
                                 (dirs tree (conj path d)))
                               (keys subtree))))
       []))))

;; ## Part 1:

(defn part1 [d]
  (->> d
       dirs
       (map #(vector (if (empty? %) "/" (last %))
                     (treesize (get-in d %))))
       (filter #(<= (last %) 100000))
       (map last)
       (apply +)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d] 
  (let [free (- 70000000 (treesize d))
        required (- 30000000 free)]
    (->> d
         dirs
         (map #(vector (or (last %) "/")
                       (treesize (get-in d %))))
         (sort-by last)
         (drop-while #(< (last %) required))
         (first))))

(part2 ex)

(part2 data)
