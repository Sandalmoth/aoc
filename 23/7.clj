(require '[clojure.string :as str])

(defn hand-type-rank [hand]
  (let [cards (seq hand)
        unique-cards (distinct cards)
        counts (sort > (map (fn [x] (count (filter #(= % x) cards))) unique-cards))]
    ; (println cards)
    ; (println unique-cards)
    ; (println counts)
    ; (println (first counts))
    (cond
      (= (first counts) 5) 6
      (= (first counts) 4) 5
      (and (= (first counts) 3) (= (second counts) 2)) 4
      (= (first counts) 3) 3
      (and (= (first counts) 2) (= (second counts) 2)) 2
      (= (first counts) 2) 1
      :else 0)
  ))

  ; (println (hand-type-rank "AAAAA"))
  ; (println (hand-type-rank "A7AAA"))
  ; (println (hand-type-rank "55AAA"))
  ; (println (hand-type-rank "52AAA"))
  ; (println (hand-type-rank "QKK3Q"))
  ; (println (hand-type-rank "QKK38"))
  ; (println (hand-type-rank "QKJ38"))

  (def hand-rank {
    \A 14
    \K 13
    \Q 12
    \J 11
    \T 10
    \9 9
    \8 8
    \7 7
    \6 6
    \5 5
    \4 4
    \3 3
    \2 2
  })
  
  (defn cmp-hands [a b]
    (let [a-rank (hand-type-rank a)
          b-rank (hand-type-rank b)]
      (if (= a-rank b-rank)
        (compare (mapv hand-rank (apply vector a)) (mapv hand-rank (apply vector b)))
        (if (< a-rank b-rank)
          -1
          1))
    ))

; (println (cmp-hands "AAAAA" "AJK77"))
; (println (cmp-hands "AJK77" "AAAAA"))
; (println (cmp-hands "QKK3Q" "KQK3Q"))
; (println (cmp-hands "KQK3Q" "QKK3Q"))

; (println (compare [1 2 3] [1 2 4]))
; (println (compare [1 2 3] [1 2 2]))

(defn cmp-entries [a b]
  (let [a-hand (first (str/split a #"\s"))
        b-hand (first (str/split b #"\s"))]
    (cmp-hands a-hand b-hand)))

(def input (sort cmp-entries (str/split-lines (slurp "input/7.txt"))))

; (println input)

(defn bid [a]
  (parse-long (second (str/split a #"\s"))))

(println (reduce + (map-indexed (fn [i x] (* (+ i 1) (bid x))) input)))

; redefine some stuff for part 2

(def hand-rank-j {
  \A 14
  \K 13
  \Q 12
  \J 1
  \T 10
  \9 9
  \8 8
  \7 7
  \6 6
  \5 5
  \4 4
  \3 3
  \2 2
})

(defn hand-type-rank-j [hand]
  (println hand)
  (if (= hand "JJJJJ") 6
    (let [cards (seq hand)
          unique-cards (distinct cards)
          ; counts (sort > (map (fn [x] (count (filter #(= % x) cards))) unique-cards))
          countmap (zipmap unique-cards (map (fn [x] (count (filter #(= % x) cards))) unique-cards))
          j-count (get countmap \J)
          countmap (dissoc countmap \J)
          counts (sort > (vals countmap))
          counts (if (nil? j-count) counts (cons (+ (first counts) j-count) (rest counts)))
          ]
      (println cards)
      (println unique-cards)
      (println counts)
      (println (first counts))
      (println j-count)
      (println counts)
      (println countmap)
      (cond
        (= (first counts) 5) 6
        (= (first counts) 4) 5
        (and (= (first counts) 3) (= (second counts) 2)) 4
        (= (first counts) 3) 3
        (and (= (first counts) 2) (= (second counts) 2)) 2
        (= (first counts) 2) 1
        :else 0)
    )))

; (println (hand-type-rank-j "AAAAA"))
; (println (hand-type-rank-j "A7AAA"))
; (println (hand-type-rank-j "55JAA"))
; (println (hand-type-rank-j "52AAA"))
; (println (hand-type-rank-j "QKK3Q"))
; (println (hand-type-rank-j "QKK38"))
; (println (hand-type-rank-j "QKJ38"))

(defn cmp-hands-j [a b]
  (let [a-rank (hand-type-rank-j a)
        b-rank (hand-type-rank-j b)]
    (if (= a-rank b-rank)
      (compare (mapv hand-rank-j (apply vector a)) (mapv hand-rank-j (apply vector b)))
      (if (< a-rank b-rank)
        -1
        1))
  ))

(defn cmp-entries-j [a b]
  (let [a-hand (first (str/split a #"\s"))
        b-hand (first (str/split b #"\s"))]
    (cmp-hands-j a-hand b-hand)))

(def input-j (sort cmp-entries-j (str/split-lines (slurp "input/7.txt"))))
(println (reduce + (map-indexed (fn [i x] (* (+ i 1) (bid x))) input-j)))
