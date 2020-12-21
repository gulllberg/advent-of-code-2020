(ns advent-of-code-2020.day-21)

(def input (slurp "src/advent_of_code_2020/inputs/day21.txt"))

(defn parse-line
  [line]
  (let [parts (clojure.string/split line #" \(")]
    [(into #{} (re-seq #"[a-z]+" (first parts)))
     (into #{} (-> (second parts)
                   (clojure.string/replace-first "contains " "")
                   (clojure.string/replace-first ")" "")
                   (clojure.string/split #", ")))]))

(defn solve-a
  []
  (let [parsed-lines (map parse-line (clojure.string/split-lines input))
        allergens (apply clojure.set/union (map second parsed-lines))
        ingredients (apply clojure.set/union (map first parsed-lines))
        ingredients-that-may-have-allergens (reduce (fn [possible-ingredients allergen]
                                                      (clojure.set/union possible-ingredients
                                                                         (->> (range (count parsed-lines))
                                                                              (filter (fn [i]
                                                                                        (contains? (second (nth parsed-lines i)) allergen)))
                                                                              (map (fn [i]
                                                                                     (first (nth parsed-lines i))))
                                                                              (apply clojure.set/intersection))))
                                                    #{}
                                                    allergens)
        ingredients-that-can't-have-allergens (clojure.set/difference ingredients ingredients-that-may-have-allergens)]
    (reduce (fn [sum ingredient]
              (+ sum (count (filter (fn [i]
                                      (contains? (first (nth parsed-lines i)) ingredient))
                                    (range (count parsed-lines))))))
            0
            ingredients-that-can't-have-allergens)))

(comment
  (solve-a)
  ; 2020
  )

(defn solve-b
  []
  (let [parsed-lines (map parse-line (clojure.string/split-lines input))
        allergens (apply clojure.set/union (map second parsed-lines))
        ingredients-that-may-have-allergens (reduce (fn [possible-ingredients allergen]
                                                      (assoc possible-ingredients allergen
                                                                                  (->> (range (count parsed-lines))
                                                                                       (filter (fn [i]
                                                                                                 (contains? (second (nth parsed-lines i)) allergen)))
                                                                                       (map (fn [i]
                                                                                              (first (nth parsed-lines i))))
                                                                                       (apply clojure.set/intersection))))
                                                    {}
                                                    allergens)]
    ingredients-that-may-have-allergens))

(comment
  (solve-b)
  ; Raw
  ; {"sesame" #{"bcdgf" "lbnmsr" "bvcrrfbr"},
  ; "peanuts" #{"xcgtv" "dhbxtb"},
  ; "wheat" #{"bcdgf" "xcgtv"},
  ; "dairy" #{"bcdgf"},
  ; "shellfish" #{"bcdgf" "dhbxtb" "scxxn"},
  ; "soy" #{"bvcrrfbr" "xhrdsl"},
  ; "nuts" #{"bcdgf" "dhbxtb" "xhrdsl" "vndrb"},
  ; "fish" #{"dhbxtb" "xhrdsl"}}

  ; Solved
  ; "dairy" #{"bcdgf"},
  ; "fish" #{"xhrdsl"}
  ; "nuts" #{"vndrb"},
  ; "peanuts" #{"dhbxtb"},
  ; "sesame" #{"lbnmsr"},
  ; "shellfish" #{"scxxn"},
  ; "soy" #{"bvcrrfbr"},
  ; "wheat" #{"xcgtv"},

  ; Answer
  ; bcdgf,xhrdsl,vndrb,dhbxtb,lbnmsr,scxxn,bvcrrfbr,xcgtv
  )
