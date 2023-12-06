(ns advent
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-file-lines [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (line-seq reader))))

;; regex matches "15" || "14 red" || "14 green" || "13 red"
(def pattern #"\b(?:1[5-9]|[2-9]\d+|14\sred|14\sgreen|13\sred)\b")

;; regex to search for numbers
(def pattern2 #"\d+")

(->> (read-file-lines "advent.txt")                ;; read file by line
     (map #(s/split % #":"))                       ;; split by ":" (Game ID vs rest)
     (filter #(nil? (re-find pattern (second %)))) ;; filter out any line that matches regex
     (map #(re-find pattern2 (first %)))           ;; get IDs from "Game X" string
     (map #(Integer/parseInt %))                   ;; convert ID strings to numbers
     (reduce +))                                   ;; get sum
