(ns advent
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-file-lines [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (line-seq reader))))

(def spelled-out-digits
  {"one"   1
   "two"   2
   "three" 3
   "four"  4
   "five"  5
   "six"   6
   "seven" 7
   "eight" 8
   "nine"  9})

(defn extract-digits-from-string [s]
  (let [spelled-out-set (set (keys spelled-out-digits))]
    (loop [s-idx       0                  ;; start-index
           e-idx       (dec (count s))    ;; end-index
           s-sub-idx   1                  ;; start-substring-index-incerement
           e-sub-idx   1                  ;; end-substring-index-decrement
           first-digit nil                
           last-digit  nil]
      (let [start-sub-range (+ s-idx s-sub-idx)                 ;; start-substring-last-index
            start-sub-str   (subs s s-idx start-sub-range)      ;; start-substring-for-lookup
            end-sub-range   (+ 1 (- e-idx e-sub-idx))           ;; end-substring-first-index
            end-sub-str     (subs s end-sub-range (+ 1 e-idx))] ;; end-substring-for-lookup
        (cond
          ;; return if both values are not nil, concatenate them into a string and return int value
          (and first-digit last-digit)
          (Integer/parseInt (str first-digit last-digit))

          ;; if start pointer reaches end; and there is still no value
          ;; set first-digit to 0 or already found last digit value
          (and (>= s-idx (dec (count s))) (nil? first-digit))
          (recur s-idx e-idx s-sub-idx e-sub-idx (or last-digit 0) last-digit)

          ;; if end pointer reaches start and there is still no value
          ;; set last-digit to 0 or already found first digit value
          (and (<= e-idx 0) (nil? last-digit))
          (recur s-idx e-idx s-sub-idx e-sub-idx first-digit (or first-digit 0))

          ;; handle when digits found
          (and (Character/isDigit (get s s-idx)) (nil? first-digit))
          (recur s-idx e-idx s-sub-idx e-sub-idx (get s s-idx) last-digit)

          (and (Character/isDigit (get s e-idx)) (nil? last-digit))
          (recur s-idx e-idx s-sub-idx e-sub-idx first-digit (get s e-idx))

          ;; keeps adding letters from next index until there is a match. e.g. "t" -> "th" -> "thr" -> "thre"
          (and (some #(s/starts-with? % start-sub-str) spelled-out-set) (nil? first-digit))
          (if (contains? spelled-out-set start-sub-str)
            (recur s-idx e-idx s-sub-idx e-sub-idx (spelled-out-digits start-sub-str) last-digit)
            (recur s-idx e-idx (inc s-sub-idx) e-sub-idx first-digit last-digit))

          ;; keeps adding letter from previous index until there is a match. e.g. "e" -> "ne" -> "one"
          (and (some #(s/ends-with? % end-sub-str) spelled-out-set)  (nil? last-digit))
          (if (contains? spelled-out-set end-sub-str)
            (recur s-idx e-idx s-sub-idx e-sub-idx first-digit (spelled-out-digits end-sub-str))
            (recur s-idx e-idx s-sub-idx (inc e-sub-idx) first-digit last-digit))

          :else
          (recur (inc s-idx) (dec e-idx) 1 1 first-digit last-digit))))))

(defn get-sum []
  (->> (read-file-lines "advent.txt")
       (map extract-digits-from-string)
       (reduce +)) ;; ->> 55686

