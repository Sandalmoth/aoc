(require '[clojure.string :as str])

(def input (str/split (str/trim (slurp "input/15.txt")) #","))

(defn HASH [s x]
  ; (println s x)
  (if (empty? s)
    x
    (let [c (int (first s))]
      (recur (rest s) (mod (* (+ x c) 17) 256)))
    ))

(println (HASH (vec "HASH") 0))

(println (reduce + 
  (map #(HASH % 0) (str/split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" #","))
  ))

; (mapv println (map #(HASH % 0) input) input)
(println (reduce + (map #(HASH % 0) input)))

