(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/3.txt")))

; (def not-symbols #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "."})
(def not-symbols #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.})
; (def not-symbols "0123456789.")

(defn symbols-in-row [x, i]
  ; (println x i)
  (if (< i (count x))
    (if (contains? not-symbols (get x i))
      (recur x (+ i 1))
      (cons i (symbols-in-row x (+ i 1))))
    ()))

; (println (symbols-in-row ".....*......*...892.........971...%....131....*..........*.." 0))
; (println (contains? not-symbols (get ".*" 0)))
; (println (contains? not-symbols (get ".*" 1)))
; (println (get ".*" 0))
; (println (get ".*" 1))
; (println (type (get ".*" 0)))
; (println (type (get ".*" 1)))

; (println (map (fn [x] [0 x]) [1 2 3]))

; (def symbol-coords (map-indexed (fn [i x]
  ; (map (fn [y] [i y]) (symbols-in-row x 0)) input)))

; (def symbol-coords (map-indexed (fn [i x] (symbols-in-row x 0)) input))
; (def symbol-coords (map-indexed (fn [i x] (map (fn [y] [y i]) (symbols-in-row x 0))) input))
; (def symbol-coords (reduce conj (map-indexed (fn [i x] (map (fn [y] [y i]) (symbols-in-row x 0))) input)))
(def symbol-coords (reduce concat 
  (map-indexed (fn [i x] (map (fn [y] [y i]) (symbols-in-row x 0))) input)))

; (println symbol-coords)

; expand the coordinates to their neighbors

(defn expand [x]
  [(map + x [-1 -1])
   (map + x [ 0 -1])
   (map + x [ 1 -1])
   (map + x [-1  0])
   (map + x [ 0  0])
   (map + x [ 1  0])
   (map + x [-1  1])
   (map + x [ 0  1])
   (map + x [ 1  1])])

(def coords-expanded (set (reduce concat (map expand symbol-coords))))

; (println coords-expanded)

; now we can easily tell whether any point in the text is next to a symbol
; by checking if [x y] is in coords-expanded

(def numbers #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn numbers-in-row [x, i]
  ; (println x i)
  (if (< i (count x))
    (if (contains? numbers (get x i))
      (cons i (numbers-in-row x (+ i 1)))
      (recur x (+ i 1))
      )
    ()))

; (println (numbers-in-row ".....*......*...892.........971...%....131....*..........*.." 0))

(def num-coords 
  (filter (fn [x] (contains? coords-expanded x)) 
    (reduce concat (map-indexed (fn [i x] (map (fn [y] [y i]) (numbers-in-row x 0))) input))))

; (println num-coords)

; (defn consecutive? [x y]
;   (println x y)
;   (and 
;     (= (last x) (last y)) 
;     (= (+ (first x)) (first y))))

(defn consecutive? [a]
  (let [x (first a) y (last a)]
    ; (println x y)
    (and 
      (= (last x) (last y)) 
      (= (+ (first x) 1) (first y)))))

(defn nonconsecutive? [a] (not (consecutive? a)))

; shoudln't be redefining i guess, but, seems inconvenient to build a single megafunction?
(def num-coords (concat num-coords [[999 999]]))
(def num-coords (map
  (fn [z] (first z)) 
    (filter nonconsecutive? (map (fn [x y] [x y]) num-coords (rest num-coords)))))

; now find the start of each number, so we can read them out easily
(defn start-of-number [xy]
  (let [x (first xy) y (last xy)]
    (cond 
      (= x 0) [0 y]
      (contains? numbers (get (get input y) (- x 1))) (start-of-number [(- x 1) y])
      :else [x y]
    )))

(def num-starts (map start-of-number num-coords))
; (println num-starts)

(defn read-number [xy]
  (let [x (first xy) 
        y (last xy)
        s (drop x (get input y))
        ss (apply str s)]
    ; (println s)
    ; (println ss)
    (read-string (re-find #"\d+" ss))))

(def nums (map read-number num-starts))
; (println nums)

(println (reduce + nums))

; (def num-coords-filtered (filter))

; (def a (map (fn [x y] 
;   [x y]) [1 2 3] [4 5 6 7]))

; (println a)

; (def num-coords (reduce (fn [x y]
;   (if (consecutive? x y) 
;     (println x y "consecutive")
;     (println x y "nonconsecutive")))
;     ; x 
;     ; (cons y x))) 
;   num-coords))

; (println num-coords)


; PART TWO!

(defn gears-in-row [x, i]
  ; (println x i)
  (if (< i (count x))
    (if (contains? #{\*} (get x i))
      (cons i (gears-in-row x (+ i 1)))
      (recur x (+ i 1)))
    ()))

(def gear-coords (reduce concat 
  (map-indexed (fn [i x] (map (fn [y] [y i]) (gears-in-row x 0))) input)))

(println gear-coords)
