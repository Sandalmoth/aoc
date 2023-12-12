(require '[clojure.string :as str])
(require '[clojure.math :as math])


(defn parse-line [l]
  (let [[springs snums] (str/split l #"\s")
        nums (map parse-long (re-seq #"\d+" snums))
        unks (count (filter #(= \? %) (seq springs)))
        ks (count (filter #(= \# %) (seq springs)))]
    ; (println springs nums unks)
    {:springs (seq springs) :nums nums :unks unks :ks ks :total (+ unks ks)}
    ))

(def input (map parse-line (str/split-lines (slurp "input/12.txt"))))

(println (first input))

; basically, use a binary representation to swap ? into . or # for 0 or 1
(defn sub-springs [springs n k result]
  ; (println springs n k result)
  (if (empty? springs) 
    result
    (if (= (first springs) \?)
      (if (>= (- n k) 0)
        (recur (rest springs) (- n k) (/ k 2) (conj result \#))
        (recur (rest springs) n (/ k 2) (conj result \.))
        )
      (recur (rest springs) n k (conj result (first springs)))
      )))

(println (let [l (first input)] 
  (sub-springs (l :springs) 13 (int (math/pow 2 (dec (l :unks)))) [])))

(defn match-pattern [springs nums]
  (let [groups (re-seq #"#+" (apply str springs))
        counts (map count groups)]
    (= counts nums)
    ))

(println (match-pattern '(\. \. \# \. \. \. \# \# \# \. \#) '(1 3 1)))

(defn num-opts [l]
  (let [k (int (math/pow 2 (dec (l :unks))))
        matches (map 
        (fn [x] (match-pattern (sub-springs (l :springs) x k []) (l :nums)))
        (range (math/pow 2 (l :unks))))]
    ; (println matches)
    (count (filter identity matches))
    ))

(println (num-opts (first input)))

(println (reduce + (map (fn [x] (println x) (num-opts x)) input)))

; maybe we can bruteforce part 2, maybe not
(defn unfold [x]
  (let [s (x :springs)
        n (x :nums)
        u (x :unks)]
    {
      :springs (concat s '(?) s '(?) s '(?) s '(?) s)
      :nums (concat n n n n n)
      :unks (* 5 u)
    }))

(println (unfold (first input)))

(println (reduce + (map #((println %) (num-opts %)) (map unfold input))))

; yeah nope, we get overflows from the (pow 2 x) which makes sense
