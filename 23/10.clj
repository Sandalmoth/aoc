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