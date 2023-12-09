(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/8.txt")))

(def desert-path (apply vector (first input)))

; (println desert-path)

(defn parse-node [l]
  (let [[node left right] (re-seq #"[A-Z]+" l)]
  [node {\L left \R right}]
  ))

(def desert-map (reduce (fn [c, x] 
  (let [[node dirs] (parse-node x)]
      (assoc c node dirs))
  ) {} (drop 2 input)))

; (println desert-map)

(defn follow [path world i pos]
  (if (= pos "ZZZ")
    i
    (let [lr (get path (mod i (count path)))]
      ; (println lr)
      ; (println (world pos))
      (recur path world (+ i 1) ((world pos) lr)))
    ))

(println (follow desert-path desert-map 0 "AAA"))

; the brute force seems to take too long

(defn allz? [poss]
  (every? true? (map (fn [x] (= (last x) \Z)) poss)))

(defn follow-ghostly [path world i poss]
  (if (allz? poss)
    i
    (let [lr (get path (mod i (count path)))
          new-poss (mapv #(% lr) (map #(world %) poss))]
      ; (println i)
      (recur path world (+ i 1) new-poss))
    ))

(def akeys (filter #(= (last %) \A) (keys desert-map)))
; (println akeys)
; (println (follow-ghostly desert-path desert-map 0 akeys))

; however, ii all ghost paths are on closed loops, there is a simpler solution
(def ghost-loops (map #(follow-ghostly desert-path desert-map 0 [%]) akeys))
(println ghost-loops)
(println (reduce * ghost-loops))
; then we just find the least common multiple of those numbers
; and that's the answer