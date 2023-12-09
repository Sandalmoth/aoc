(require '[clojure.string :as str])

(defn to-nums [x]
  (map parse-long (re-seq #"-?\d+" x)))

(def input (map to-nums (str/split-lines (slurp "input/9.txt"))))

; (println (first input))

(defn dseq [x]
  (map - (rest x) x))

; (println (dseq (first input)))

(defn allz? [x]
  (or (= (count x) 0) (every? #(= 0 %) x)))

; (println (allz? [1 2 3]))
; (println (allz? [0 0 0]))

(defn oasis-predict [x]
  ; (println x)
  (if (allz? x)
    0
    (+ (last x) (oasis-predict (dseq x)))))

; (println (oasis-predict [0 3 6 9 12 15]))
; (println (oasis-predict [1 3 6 10 15 21]))
; (println (oasis-predict [10 13 16 21 30 45]))

(println (reduce + (map oasis-predict input)))
(println (reduce + (map oasis-predict (map reverse input))))

