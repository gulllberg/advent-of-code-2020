(ns advent-of-code-2020.day-23
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [index-of]]))

(def input "315679824")

(defn get-next-state
  [remaining-cups picked-up-cups current-cup]
  (let [destination-cup-index (loop [cup-to-look-for (str (dec (read-string current-cup)))]
                                (if-let [index (clojure.string/index-of remaining-cups cup-to-look-for)]
                                  index
                                  (if (= "0" cup-to-look-for)
                                    (recur "9")
                                    (recur (str (dec (read-string cup-to-look-for)))))))]
    (str (subs remaining-cups 0 (inc destination-cup-index)) picked-up-cups (subs remaining-cups (inc destination-cup-index)))))

(defn get-next-current-cup
  {:test (fn []
           (is= (get-next-current-cup "328915467" "3") "2"))}
  [cups current-cup]
  (let [current-cup-index (clojure.string/index-of cups current-cup)]
    (if (= current-cup-index 8)
      (subs cups 0 1)
      (subs cups (inc current-cup-index) (+ current-cup-index 2)))))

(defn play-game
  {:test (fn []
           (is= (play-game 10 "389125467" "3") "583741926"))}
  [rounds cups starting-cup]
  (loop [i 0
         cups cups
         current-cup starting-cup]
    (if (= i rounds)
      cups
      (let [current-index (clojure.string/index-of cups current-cup)]
        (cond
          (= current-index 8)
          (let [next-cup-state (get-next-state (subs cups 3) (subs cups 0 3) current-cup)]
            (recur (inc i) next-cup-state (get-next-current-cup next-cup-state current-cup)))

          (= current-index 7)
          (let [next-cup-state (get-next-state (subs cups 2 8) (str (subs cups 8) (subs cups 0 2)) current-cup)]
            (recur (inc i) next-cup-state (get-next-current-cup next-cup-state current-cup)))

          (= current-index 6)
          (let [next-cup-state (get-next-state (subs cups 1 7) (str (subs cups 7) (subs cups 0 1)) current-cup)]
            (recur (inc i) next-cup-state (get-next-current-cup next-cup-state current-cup)))

          :else
          (let [next-cup-state (get-next-state (str (subs cups 0 (inc current-index)) (subs cups (+ current-index 4)))
                                               (subs cups (inc current-index) (+ current-index 4))
                                               current-cup)]
            (recur (inc i) next-cup-state (get-next-current-cup next-cup-state current-cup))))))))

(defn get-answer
  [cups]
  (let [i (clojure.string/index-of cups "1")]
    (str (subs cups (inc i)) (subs cups 0 i))))

(defn solve-a
  []
  (get-answer (play-game 100 input "3")))

(comment
  (solve-a)
  ; 72496583
  )

;(defn create-starting-cup-state
;  [input max-number]
;  (vec (concat (map read-string (re-seq #"\d" input)) (range 10 (inc max-number)))))

; All numbers keep track of the one they have after them
(defn create-starting-cup-state
  [input max-number]
  (let [numbers (concat (map read-string (re-seq #"\d" input)) (range (inc (count input)) (inc max-number)))]
    (reduce (fn [state i]
              (if (= i (dec (count numbers)))
                (assoc state (nth numbers i) (first numbers))
                (assoc state (nth numbers i) (nth numbers (inc i)))))
            ; Label 0 does not exist, so put nil there and for all others index = label
            (assoc (vec (range (inc max-number))) 0 nil)
            (range (count numbers)))))

(defn pick-up-cups
  {:test (fn []
           (is= (pick-up-cups [nil 2 5 8 6 4 7 3 9 1] 3) [[nil nil 5 2 6 4 7 3 nil nil] [8 9 1]]))}
  [cups current-cup]
  (let [[remaining-cups picked-up-cups next-pointer] (reduce (fn [[remaining-cups picked-up-cups next-pointer] _]
                                                               [(assoc remaining-cups next-pointer nil) (conj picked-up-cups next-pointer) (nth remaining-cups next-pointer)])
                                                            [cups [] (nth cups current-cup)]
                                                            (range 3))]
    ; Glue together remaining cups again
    [(assoc remaining-cups current-cup next-pointer) picked-up-cups]))

(defn put-back-picked-up-cups
  {:test (fn []
           (is= (put-back-picked-up-cups [nil nil 5 2 6 4 7 3 nil nil] [8 9 1] 2) [nil 5 8 2 6 4 7 3 9 1]))}
  [cups picked-up-cups destination-cup]
  (reduce (fn [[new-cups next] i]
            (if (= i 3)
              ; Make the last of picked up cups point at the one destination-cup originally pointed to
              (assoc new-cups next (nth cups destination-cup))
              [(assoc new-cups next (nth picked-up-cups i)) (nth picked-up-cups i)]))
          [cups destination-cup]
          (range 4)))

(defn get-next-state-b
  {:test (fn []
           (is= (get-next-state-b [nil nil 5 2 6 4 7 3 nil nil] [8 9 1] 3 9) [nil 5 8 2 6 4 7 3 9 1]))}
  [remaining-cups picked-up-cups current-cup max-label]
  (let [destination-cup (loop [cup-to-look-for (dec current-cup)]
                          (if (nth remaining-cups cup-to-look-for)
                            cup-to-look-for
                            (if (= 0 cup-to-look-for)
                              (recur max-label)
                              (recur (dec cup-to-look-for)))))]
    (put-back-picked-up-cups remaining-cups picked-up-cups destination-cup)))

(defn play-game-b
  {:test (fn []
           (is= (play-game-b 10 (create-starting-cup-state "389125467" 9) 3) (create-starting-cup-state "583741926" 9)))}
  [rounds cups starting-cup]
  (let [max-label (dec (count cups))]
    (loop [i 0
           cups cups
           current-cup starting-cup]
      (if (= i rounds)
        cups
        (let [[remaining-cups picked-up-cups] (pick-up-cups cups current-cup)
              next-cup-state (get-next-state-b remaining-cups picked-up-cups current-cup max-label)]
          (recur (inc i) next-cup-state (nth next-cup-state current-cup)))))))


;(defn slice
;  {:test (fn []
;           (is= (slice [1 2 3 4 5] 2 4) [3 4])
;           (is= (slice '(1 2 3 4 5) 2 4) [3 4])
;           (is= (slice [1 2 3 4 5] 0 2) [1 2])
;           (is= (slice [1 2 3 4 5] 2) [3 4 5])
;           (is= (slice '(1 2 3 4 5) 2) [3 4 5]))}
;  ([coll start]
;   (drop start coll))
;  ([coll start end]
;   (drop start (take end coll))))

;(defn get-next-state-b
;  [remaining-cups picked-up-cups current-cup max-index]
;  (let [destination-cup-index (loop [cup-to-look-for (dec current-cup)]
;                                (if-let [index (index-of remaining-cups cup-to-look-for)]
;                                  index
;                                  (if (= 0 cup-to-look-for)
;                                    (recur (inc max-index))
;                                    (recur (dec cup-to-look-for)))))]
;    (vec (concat (subvec remaining-cups 0 (inc destination-cup-index)) picked-up-cups (subvec remaining-cups (inc destination-cup-index))))))

;(defn get-next-current-cup-b
;  {:test (fn []
;           (is= (get-next-current-cup-b (create-starting-cup-state "328915467" 9) 3 8) 2))}
;  [cups current-cup max-index]
;  (let [current-cup-index (index-of cups current-cup)]
;    (if (= current-cup-index max-index)
;      (first cups)
;      (nth cups (inc current-cup-index)))))

;(defn play-game-b
;  {:test (fn []
;           (is= (play-game-b 10 (create-starting-cup-state "389125467" 9) 3) (create-starting-cup-state "583741926" 9)))}
;  [rounds cups starting-cup]
;  (let [max-index (dec (count cups))]
;    (loop [i 0
;           cups cups
;           current-cup starting-cup]
;      (if (= i rounds)
;        cups
;        (let [current-index (index-of cups current-cup)]
;          (cond
;            (= current-index max-index)
;            (let [next-cup-state (get-next-state-b (slice cups 3) (slice cups 0 3) current-cup max-index)]
;              (recur (inc i) next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)))
;
;            (= current-index (dec max-index))
;            (let [next-cup-state (get-next-state-b (slice cups 2 max-index) (concat (slice cups max-index) (slice cups 0 2)) current-cup max-index)]
;              (recur (inc i) next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)))
;
;            (= current-index (- max-index 2))
;            (let [next-cup-state (get-next-state-b (slice cups 1 (dec max-index)) (concat (slice cups (dec max-index)) (slice cups 0 1)) current-cup max-index)]
;              (recur (inc i) next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)))
;
;            :else
;            (let [next-cup-state (get-next-state-b (concat (slice cups 0 (inc current-index)) (slice cups (+ current-index 4)))
;                                                   (slice cups (inc current-index) (+ current-index 4))
;                                                   current-cup
;                                                   max-index)]
;              (recur (inc i) next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)))))))))

;(defn play-game-b
;  {:test (fn []
;           (is= (play-game-b 10 (create-starting-cup-state "389125467" 9) 3) (create-starting-cup-state "583741926" 9)))}
;  [rounds cups starting-cup]
;  (let [max-index (dec (count cups))]
;    (first (reduce (fn [[cups current-cup] i]
;                     (when (= 0 (mod i 100000))
;                       (println i))
;                     ;(println cups)
;                     (let [current-index (index-of cups current-cup)]
;                       (cond
;                         (= current-index max-index)
;                         (let [next-cup-state (get-next-state-b (subvec cups 3) (subvec cups 0 3) current-cup max-index)]
;                           [next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)])
;
;                         (= current-index (dec max-index))
;                         (let [next-cup-state (get-next-state-b (subvec cups 2 max-index) (vec (concat (subvec cups max-index) (subvec cups 0 2))) current-cup max-index)]
;                           [next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)])
;
;                         (= current-index (- max-index 2))
;                         (let [next-cup-state (get-next-state-b (subvec cups 1 (dec max-index)) (vec (concat (subvec cups (dec max-index)) (subvec cups 0 1))) current-cup max-index)]
;                           [next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)])
;
;                         :else
;                         (let [next-cup-state (get-next-state-b (vec (concat (subvec cups 0 (inc current-index)) (subvec cups (+ current-index 4))))
;                                                                (subvec cups (inc current-index) (+ current-index 4))
;                                                                current-cup
;                                                                max-index)]
;                           [next-cup-state (get-next-current-cup-b next-cup-state current-cup max-index)]))))
;                   [cups starting-cup]
;                   (range rounds)))))


(defn get-answer-b
  {:test (fn []
           (is= (get-answer-b {7 4, 1 9, 4 1, 6 5, 3 7, 2 6, 9 2, 5 8, 8 3}) 18))}
  [cups]
  (let [cup-after-1 (get cups 1)
        cup-after-that (get cups cup-after-1)]
    (* cup-after-1 cup-after-that)))

(defn solve-b
  []
  (get-answer-b (play-game-b 10000000 (create-starting-cup-state input 1000000) 3)))

(comment
  (solve-b)
  (time
    (get-answer-b (play-game-b 1000 (create-starting-cup-state "389125467" 1000) 3)))
  ;
  )
