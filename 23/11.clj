(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/11.txt")))

(defn galaxies-in-row [x, i]
  (if (< i (count x))
    (if (contains? #{\#} (get x i))
      (cons i (galaxies-in-row x (+ i 1)))
      (recur x (+ i 1)))
    ()))

(def galaxy-coords (reduce concat 
  (map-indexed (fn [i x] (map (fn [y] [y i]) (galaxies-in-row x 0))) input)))

(println galaxy-coords)

(def galaxy-cols (reduce #(conj %1 (first %2)) #{} galaxy-coords))
(def galaxy-rows (reduce #(conj %1 (second %2)) #{} galaxy-coords))

(println galaxy-cols)
(println galaxy-rows)

; part 1
; (defn expand [x, space]
  ; (+ x (count (filter #(not (contains? space %)) (range x)))))

; part 2
(defn expand [x, space]
  (+ x (* 999999 (count (filter #(not (contains? space %)) (range x))))))


(defn expand-coordinate [xy]
  (let [[x y] xy]
    [(expand x galaxy-cols) (expand y galaxy-rows)]
    ))

(def galaxy-coords-expanded (map expand-coordinate galaxy-coords))

(println galaxy-coords-expanded)

(defn distance [[ax ay] [bx by]]
  ; (println [ax ay] [bx by] (+ (abs (- ax bx)) (abs (- ay by))))
  (+ (abs (- ax bx)) (abs (- ay by))))

(println (/ 
  (reduce + (map 
    #(distance (first %) (second %))
    (for [a galaxy-coords-expanded b galaxy-coords-expanded] [a b])))
  2))
