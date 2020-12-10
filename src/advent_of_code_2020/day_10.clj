(ns advent-of-code-2020.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2020/inputs/day10.txt"))

(defn solve-a
  []
  (let [adapters (sort (map read-string (clojure.string/split-lines input)))]
    (apply * (reduce (fn [[n-1-diff n-3-diff] i]
                       (let [diff (condp = i
                                    0 (nth adapters i)
                                    (count adapters) 3
                                    (- (nth adapters i) (nth adapters (dec i))))]
                         (condp = diff
                           1 [(inc n-1-diff) n-3-diff]
                           3 [n-1-diff (inc n-3-diff)]
                           [n-1-diff n-3-diff])))
                     [0 0]
                     (range (inc (count adapters)))))))

(comment
  (solve-a)
  ; 1885
  )

(defn get-valid-next-i
  {:test (fn []
           (is= (get-valid-next-i [149
                                   148
                                   147
                                   146
                                   145
                                   142
                                   139]
                                  0)
                [])
           (is= (get-valid-next-i [149
                                   148
                                   147
                                   146
                                   145
                                   142
                                   139]
                                  5)
                [4]))}
  [adapters i]
  (filter (fn [j]
            (and (>= j 0)
                 (>= 3 (- (nth adapters j) (nth adapters i)))))
          (range (- i 3) i)))

(defn solve-b
  []
  (let [adapters (concat (reverse (sort (map read-string (clojure.string/split-lines input)))) [0])]
    (last (reduce (fn [n-possible-arrangements i]
                    (assoc n-possible-arrangements i (apply + (map (fn [j]
                                                                     (nth n-possible-arrangements j))
                                                                   (get-valid-next-i adapters i)))))
                  (assoc (vec (range (count adapters))) 0 1)
                  (range 1 (count adapters))))))

(comment
  (solve-b)
  ; 2024782584832
  )
