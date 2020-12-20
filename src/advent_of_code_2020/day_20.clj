(ns advent-of-code-2020.day-20)

(def input (slurp "src/advent_of_code_2020/inputs/day20.txt"))

(defn parse-tile
  [tile]
  (let [lines (clojure.string/split-lines tile)
        id (re-find #"\d+" (first lines))]
    [id (second lines) (last lines) (apply str (map first (rest lines))) (apply str (map last (rest lines)))]))

(defn create-tile-state
  [tiles]
  (reduce (fn [state tile]
            (let [parsed-tile (parse-tile tile)]
              (assoc state (first parsed-tile) (rest parsed-tile))))
          {}
          tiles))

(defn check-if-possible-neighbours
  [tile-1-edges tile-2-edges]
  (some (fn [[e-1 e-2]]
          (= e-1 e-2))
        (for [e-1 [(first tile-1-edges) (second tile-1-edges) (nth tile-1-edges 2) (nth tile-1-edges 3)
                   (apply str (reverse (first tile-1-edges))) (apply str (reverse (second tile-1-edges))) (apply str (reverse (nth tile-1-edges 2))) (apply str (reverse (nth tile-1-edges 3)))]
              e-2 [(first tile-2-edges) (second tile-2-edges) (nth tile-2-edges 2) (nth tile-2-edges 3)
                   (apply str (reverse (first tile-2-edges))) (apply str (reverse (second tile-2-edges))) (apply str (reverse (nth tile-2-edges 2))) (apply str (reverse (nth tile-2-edges 3)))]]
          [e-1 e-2])))

(defn count-possible-neighbours
  [tile-state]
  (let [ids (keys tile-state)]
    (loop [i 0
           j 1
           count-state (reduce (fn [s id]
                                 (assoc s id 0))
                               {}
                               ids)]
      (cond
        (= i (dec (count ids)))
        count-state

        (= j (count ids))
        (recur (inc i) (+ i 2) count-state)

        :else (if (check-if-possible-neighbours (get tile-state (nth ids i)) (get tile-state (nth ids j)))
                (recur i (inc j) (-> count-state
                                     (update (nth ids i) inc)
                                     (update (nth ids j) inc)))
                (recur i (inc j) count-state))))))

(defn solve-a
  []
  (let [count-state (count-possible-neighbours (create-tile-state (clojure.string/split input #"\n\n")))]
    (apply * (map read-string (keys (filter (fn [[k v]]
                                              (= v 2))
                                            count-state))))))

(comment
  (solve-a)
  ; 18449208814679
  )
