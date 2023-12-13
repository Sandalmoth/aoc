(require '[clojure.string :as str])

(defn parse-ash [input acc tmp]
  (cond
    (empty? input) (conj acc tmp)
    (zero? (count (first input))) (recur (rest input) (conj acc tmp) [])
    :else (recur (rest input) acc (conj tmp (apply vector (first input))))
    ))

(def input (parse-ash (str/split-lines (slurp "input/13.txt")) [] []))

; (println (first input))

(defn vmirr? [ash x]
  (every? identity (mapv = (drop x ash) (reverse (take x ash)))))

; (println (vmirr? (first input) 6))

(defn transpose [m]
  (apply mapv vector m))

; (println (transpose (first input)))

(defn find-vmirr [ash n]
    (cond 
      (= n (count ash)) nil
      (vmirr? ash n) n
      :else (recur ash (+ n 1))
      ))

; (println (find-vmirr (first input) 1))
; (println (find-vmirr (transpose (first input)) 1))

; (println (find-vmirr (second input) 1))
; (println (find-vmirr (transpose (second input)) 1))

(def vmirrs (map #(find-vmirr % 1) input))
(def hmirrs (map #(find-vmirr (transpose %) 1) input))

(println (+ 
  (reduce + (filter identity hmirrs)) 
  (reduce + (map #(* 100 %) (filter identity vmirrs)))))
