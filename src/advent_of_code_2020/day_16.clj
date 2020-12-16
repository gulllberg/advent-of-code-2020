(ns advent-of-code-2020.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def your-ticket-input "137,173,167,139,73,67,61,179,103,113,163,71,97,101,109,59,131,127,107,53")
(def rules-input (slurp "src/advent_of_code_2020/inputs/day16rules.txt"))
(def nearby-tickets-input (slurp "src/advent_of_code_2020/inputs/day16nearby.txt"))

(defn is-value-valid
  {:test (fn []
           (is (is-value-valid 35 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is (is-value-valid 500 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is (is-value-valid 800 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is (is-value-valid 900 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is-not (is-value-valid 1000 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is-not (is-value-valid 1 [[[33 430] [456 967]] [[42 864] [875 957]]])))}
  [value rules]
  (reduce (fn [is-valid rule]
            (if (or (<= (nth (nth rule 0) 0) value (nth (nth rule 0) 1))
                        (<= (nth (nth rule 1) 0) value (nth (nth rule 1) 1)))
              (reduced true)
              false))
          false
          rules))

(defn solve-a
  []
  (let [rules (reduce (fn [a rule]
                        (let [[f s] (re-seq #"\d+-\d+" rule)]
                          (conj a [(map read-string (re-seq #"\d+" f)) (map read-string (re-seq #"\d+" s))])))
                      []
                      (clojure.string/split-lines rules-input))]
    (reduce (fn [error-rate ticket]
              (+ error-rate (reduce (fn [a v]
                                      (let [number (read-string v)]
                                        (if-not (is-value-valid number rules)
                                          (+ a number)
                                          a)))
                                    0
                                    (clojure.string/split ticket #","))))
            0
            (clojure.string/split-lines nearby-tickets-input))))

(comment
  (solve-a)
  ; 26026
  )
