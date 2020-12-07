(ns aoc.day1
  (:require [clojure.string :as str]))

(defn -two-sum-part1-rec
  [numbers acc]
  (let [x (first numbers)
        y (acc (- 2020 x))]
    (if y
      [x y]
      (recur (rest numbers) (conj acc x)))))

(defn two-sum-part1
  [numbers]
  (let [[a b] (-two-sum-part1-rec numbers #{})]
    (* a b)))

(defn read-file
  [name]
  (map #(Integer/parseInt %) (str/split-lines (slurp name))))

(defn solve
  []
  (let [input (read-file "data/input1.txt")]
    (println (two-sum-part1 input))))
