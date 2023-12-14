(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/14.txt")))

(def n-rules {
  [\. \.] \.
  [\. \O] \O
  [\. \#] \.
  [\O \.] \O
  [\O \O] \O
  [\O \#] \O
  [\# \.] \#
  [\# \O] \#
  [\# \#] \#
})

(def s-rules {
  [\. \.] \.
  [\. \O] \.
  [\. \#] \#
  [\O \.] \.
  [\O \O] \O
  [\O \#] \#
  [\# \.] \.
  [\# \O] \O
  [\# \#] \#
})

(defn slide-n [n s]
  (let [n2 (apply str (map #(n-rules [%1 %2]) (seq n) (seq s)))
        s2 (apply str (map #(s-rules [%1 %2]) (seq n) (seq s)))]
    ; (println n2)
    ; (println s2)
    [n2 s2]))

; (println (slide-n (first input) (second input)))

(defn slide-table-n [inp out]
  (cond 
    (empty? inp) out
    (nil? (second inp)) (conj out (first inp))
    :else (let [[n2 s2] (slide-n (first inp) (second inp))]
      (recur (concat [s2] (drop 2 inp)) (conj out n2)))
    ))

; (mapv println (slide-table-n input []))
; (println (count (slide-table-n input [])))

(defn slide-complete-n [inp n]
  (if (zero? n)
    inp
    (recur (slide-table-n inp []) (- n 1))
    ))

; (mapv println (slide-complete-n input 123)) ; 100 should be all we need but whatever

(def input-n (slide-complete-n input 123))

(defn count-O [l]
  (count (re-seq #"O" l)))

(println (reduce + (map-indexed (fn [i l] (* (- 100 i) (count-O l))) input-n)))