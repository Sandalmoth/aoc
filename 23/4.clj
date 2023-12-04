(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/4.txt")))

; ugly regex...
(defn winner-set [card]
  (let [m (rest (re-find #"\:\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+\|" card))] 
    (assert (= (count m) 10))
    (set (map parse-long m))))

; (println (mapv winner-set input))

(defn score [n] 
  (if (= n 0)
    0
    (int (Math/pow 2 (- n 1)))))

(println (score 0))
(println (score 1))
(println (score 2))
(println (score 3))
(println (score 4))

(defn score-card [card]
  (let [winners (winner-set card)
        nums (map parse-long (-> card 
          (str/split #"\|")
          (last)
          (str/trim)
          (str/split #"\s+")))
        n (count (filter (fn [x] (contains? winners x)) nums))]
          (score n)))

(println (score-card (first input)))
(println (map score-card input))
(println (reduce + (map score-card input)))

(defn n-hits [card]
  (let [winners (winner-set card)
        nums (map parse-long (-> card 
          (str/split #"\|")
          (last)
          (str/trim)
          (str/split #"\s+")))
        n (count (filter (fn [x] (contains? winners x)) nums))]
        n))

; (map-indexed println '(1 2 3))

(def hits (mapv n-hits input))
(println hits)

(defn score-card-2 [i]
  (let [n (get hits i)
        dups (map #(+ i 1 %) (range n))]
    ; (println dups)
    (if (empty? dups) 
      1
      (+ 1 (reduce + (map score-card-2 dups))))))
  ; (println i n)
  ; (if (> n 0)
  ;   (reduce + ())
  ;   1))

; (def a (map score-card-2 (range (count (hits)))))
; (println a)

(println (score-card-2 0))
(println (map score-card-2 (range (count hits))))
(println (reduce + (map score-card-2 (range (count hits)))))
