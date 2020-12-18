(ns advent-of-code-2020.day-18
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2020/inputs/day18.txt"))

(defn compute-result
  {:test (fn []
           (is= (compute-result ["1" "2" "3" "4" "5" "6"] ["+" "*" "+" "*" "+"]) 71))}
  [numbers operators]
  (reduce (fn [r i]
            (eval (read-string (str "(" (nth operators i) " " r " " (nth numbers (inc i)) ")"))))
          (read-string (first numbers))
          (range (count operators))))

(defn evaluate-line
  {:test (fn []
           (is= (evaluate-line "1 + 2 * 3 + 4 * 5 + 6") 71)
           (is= (evaluate-line "1 + (2 * 3) + (4 * (5 + 6))") 51))}
  [line]
  (loop [i 0
         depth 0
         curr nil
         operators {0 []}
         numbers {0 []}]
    (if (= i (count line))
      (compute-result (concat (get numbers 0) (if curr [curr] [])) (get operators 0))
      (let [c (str (nth line i))]
        (condp = c
          " " (if curr
                (recur (inc i) depth nil operators (update numbers depth conj curr))
                (recur (inc i) depth nil operators numbers))
          "+" (recur (inc i) depth nil (update operators depth conj "+") numbers)
          "*" (recur (inc i) depth nil (update operators depth conj "*") numbers)
          "(" (recur (inc i) (inc depth) nil (assoc operators (inc depth) []) (assoc numbers (inc depth) []))
          ")" (recur (inc i) (dec depth) nil (dissoc operators depth) (-> numbers
                                                                          (dissoc depth)
                                                                          (update (dec depth) conj (str (compute-result (concat (get numbers depth) (if curr [curr] [])) (get operators depth))))))
          ; a digit
          (recur (inc i) depth (str curr c) operators numbers))))
    ))

(defn solve-a
  []
  (apply + (map evaluate-line (clojure.string/split-lines input))))

(comment
  (solve-a)
  ; 510009915468
  )

(defn compute-result-b
  {:test (fn []
           (is= (compute-result-b ["1" "2" "3" "4" "5" "6"] ["+" "*" "+" "*" "+"]) 231))}
  [numbers operators]
  (loop [i 0
         ns []
         os []
         n (first numbers)]
    (if (= i (count operators))
      (compute-result (conj ns n) os)
      (let [operator (nth operators i)]
        (if (= operator "+")
          (recur (inc i) ns os (str (eval (read-string (str "(+ " n " " (nth numbers (inc i)) ")")))))
          (recur (inc i) (conj ns n)  (conj os operator) (nth numbers (inc i))))))))

(defn evaluate-line-b
  {:test (fn []
           (is= (evaluate-line-b "1 + 2 * 3 + 4 * 5 + 6") 231)
           (is= (evaluate-line-b "1 + (2 * 3) + (4 * (5 + 6))") 51))}
  [line]
  (loop [i 0
         depth 0
         curr nil
         operators {0 []}
         numbers {0 []}]
    (if (= i (count line))
      (compute-result-b (concat (get numbers 0) (if curr [curr] [])) (get operators 0))
      (let [c (str (nth line i))]
        (condp = c
          " " (if curr
                (recur (inc i) depth nil operators (update numbers depth conj curr))
                (recur (inc i) depth nil operators numbers))
          "+" (recur (inc i) depth nil (update operators depth conj "+") numbers)
          "*" (recur (inc i) depth nil (update operators depth conj "*") numbers)
          "(" (recur (inc i) (inc depth) nil (assoc operators (inc depth) []) (assoc numbers (inc depth) []))
          ")" (recur (inc i) (dec depth) nil (dissoc operators depth) (-> numbers
                                                                          (dissoc depth)
                                                                          (update (dec depth) conj (str (compute-result-b (concat (get numbers depth) (if curr [curr] [])) (get operators depth))))))
          ; a digit
          (recur (inc i) depth (str curr c) operators numbers))))
    ))

(defn solve-b
  []
  (apply + (map evaluate-line-b (clojure.string/split-lines input))))

(comment
  (solve-b)
  ; 321176691637769
  )
