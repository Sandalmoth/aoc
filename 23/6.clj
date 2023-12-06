(defn solve-race [time, record]
  (let [options (range (+ time 1))
        distances (map (fn [x] (* x (- time x))) options)]
    ; (println options)
    ; (println distances)
    (count (filter #(> % record) distances))
  ))

; input is so small, i cant be bothered to write a parser
(def races [
  [62 553] [64 1010] [91 1473] [90 1074]
])

(println (reduce * (map #(apply solve-race %) races)))

; slow-ish, but still very doable (unlike the problem 5 brute-force)
(println (solve-race 62649190 553101014731074))

