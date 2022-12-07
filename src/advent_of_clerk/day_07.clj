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
         [line & rest :as lines] (str/split-lines s)]

    (cond
      (or (not line) (empty? line))
      ;; End of input
      tree

      (and ls? (str/starts-with? line "$ "))
      ;; End of ls output
      (recur pwd tree false lines)

      (and ls? (str/starts-with? line "dir "))
      ;; Directory in ls output
      (let [[_ fname] (str/split line #" ")
            newtree (assoc-in tree (conj pwd fname) {})]
        (recur pwd newtree ls? rest))

      ls?
      ;; Other ls output (i.e. a file)
      (let [[size fname] (str/split line #" ")
            size (parse-long size)
            newtree (assoc-in tree (conj pwd fname) size)]
        (recur pwd newtree ls? rest))

      (str/starts-with? line "$ cd ")
      ;; Update directory
      (let [newpath (cd pwd (subs line 5))]
        (recur newpath tree ls? rest))

      (= line "$ ls")
      ;; Start of ls output
      (recur pwd tree true rest)

      :else
      ;; Shouldn't happen
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

;; Total size of a (sub)tree:
(defn treesize [tree]
  (cond
    (map? tree)
    (apply + (map treesize (vals tree)))

    (int? tree)
    tree

    :else
    (throw (Exception. (format "oops: %s" tree)))))

;; All (sub)dirs in a tree:
(defn dirs
  ([tree]
   (dirs tree []))
  ([tree path]
   (let [subtree (get-in tree path)]
     (if (map? subtree)
       ;; It's a dir: add this path plus all subdirs
       (cons path
             (mapcat #(dirs tree (conj path %))
                     (keys subtree)))
       ;; It's not a dir, don't add anything
       []))))

;; ## Part 1:

(defn part1 [d]
  (->> d
       (dirs)
       (map (partial get-in d))
       (map treesize)
       (filter #(<= % 100000))
       (apply +)))

(part1 ex)

(part1 data)

;; ## Part 2:

(defn part2 [d]
  (let [free (- 70000000 (treesize d))
        required (- 30000000 free)]
    (->> d
         (dirs)
         (map (partial get-in d))
         (map treesize)
         (sort)
         (drop-while #(< % required))
         (first))))

(part2 ex)

(part2 data)
