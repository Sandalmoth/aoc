(require '[clojure.string :as str])

(def start-pipe \-) ; by manual examination
(def start-pipe-xy [100 52])

(def input (str/split-lines (slurp "input/10.txt")))

(defn pipe-at [xy]
  (let [[x y] xy]
    (get (get input y) x)
    ))

(assert (= (pipe-at [100 52]) \S))
; (println (pipe-at [0 0]))
; (println (pipe-at [1 1]))

; maps from-dir to from-dir
(def from-dirs {
  \| {:n :n :s :s}
  \- {:e :e :w :w}
  \L {:n :w :e :s}
  \J {:n :e :w :s}
  \7 {:s :e :w :n}
  \F {:s :w :e :n}
  \S {:e :e :w :w}
  \. {}
})

; maps from-dir to change in coordinate
(def from-coords {
  \| {:n [0 1] :s [0 -1]}
  \- {:e [-1 0] :w [1 0]}
  \L {:n [1 0] :e [0 -1]}
  \J {:n [-1 0] :w [0 -1]}
  \7 {:s [-1 0] :w [0 1]}
  \F {:s [1 0] :e [0 1]}
  \S {:e [-1 0] :w [1 0]}
  \. {}
})

(defn follow-at [xy from]
  (let [pipe (pipe-at xy)
        nf ((from-dirs pipe) from)
        cc ((from-coords pipe) from)]
    [(map + xy cc) nf]
    ))

; (println (follow-at [0 0] :s))
; (println (follow-at [0 0] :e))

(defn follow-path [xy from history]
  ; (println xy from history)
  (if (= xy start-pipe-xy)
    history
    (let [pipe (pipe-at xy)
          nf ((from-dirs pipe) from)
          cc ((from-coords pipe) from)]
      (recur (mapv + xy cc) nf (conj history xy)))
    ))

(def pipe-loop (conj (follow-path [101 52] :w []) start-pipe-xy))
(println pipe-loop)
(println (count pipe-loop)) ; div by 2 to get answer (6823)


; one way of checking for being inside a shape is 
; to see how many crossings there are on the way out
; and if it's odd we're inside
; the alignment of gaps and pipes makes ita awkward
; but maybe we can just move horizontally and ignore - on the path

(defn not-h? [x] (not (or (= x \-) (= x \S))))

(def pipe-loop-set (set (filter #(not-h? (pipe-at %)) pipe-loop)))

(println (contains? pipe-loop-set [100 52]))


(defn count-left [xy]
  (let [[x y] xy]
    (reduce + (map #(if % 1 0) 
      (map #(contains? pipe-loop-set [% y]) (range x))
      ))))

(println (count-left start-pipe-xy))

; (println (mapv #(vec [%1 %2]) (range 3) (range 3)))
(println (for [a (range 3) b (range 3)] [a b]))

(def candidates (filter #(not (contains? pipe-loop-set %)) (for [a (range 140) b (range 140)] [a b])))
; (println candidates)

(def inside (filter #(odd? (count-left %)) candidates))
(def outside (filter #(even? (count-left %)) candidates))
(println (count inside))




