(ns advent-of-code-2020.day-15)

(def input [1 0 18 10 19 6])

(defn process-input
  [input]
  (reduce (fn [a i]
            (assoc a (nth input i) [i nil] :last (nth input i)))
          {}
          (range (count input))))

(defn solve-for-number
  [number]
  (loop [i 6
         s (process-input input)]
    (if (= i number)
      (:last s)
      (recur (inc i)
             (let [last-number (:last s)
                   [last-appearance penultimate-appearance] (get s last-number)]
               (if penultimate-appearance
                 (let [new-number (- last-appearance penultimate-appearance)]
                   (assoc s new-number (if (contains? s new-number) [i (get-in s [new-number 0])] [i nil]) :last new-number))
                 (assoc s 0 [i (get-in s [0 0])] :last 0)))))))

(defn solve-a
  []
  (solve-for-number 2020))

(comment
  (time
    (solve-a))
  ; 441
  )

(defn solve-b
  []
  (solve-for-number 30000000))

(comment
  (time
    (solve-b))
  ; 10613991
  ; "Elapsed time: 37000.796707 msecs"
  )
