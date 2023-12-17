(require '[clojure.string :as str])
(require '[clojure.set :as s])

(def input (str/split-lines (slurp "input/16.txt")))

; (println input)
; (mapv println input)

(defn energize [xy d result]
  (let [[x y] xy
        t (get (get input y) x)]
      ; (println result)
      ; (println x y d t)
      (cond
        (nil? t) result
        (contains? result [x y d]) result
        ;
        (and (= d :n) (= t \.)) (recur (map + [0 -1] xy) :n (conj result [x y d]))
        (and (= d :s) (= t \.)) (recur (map + [0 1] xy) :s (conj result [x y d]))
        (and (= d :w) (= t \.)) (recur (map + [-1 0] xy) :w (conj result [x y d]))
        (and (= d :e) (= t \.)) (recur (map + [1 0] xy) :e (conj result [x y d]))
        ;
        (and (= d :n) (= t \/)) (recur (map + [1 0] xy) :e (conj result [x y d]))
        (and (= d :n) (= t \\)) (recur (map + [-1 0] xy) :w (conj result [x y d]))
        (and (= d :s) (= t \\)) (recur (map + [1 0] xy) :e (conj result [x y d]))
        (and (= d :s) (= t \/)) (recur (map + [-1 0] xy) :w (conj result [x y d]))
        (and (= d :w) (= t \/)) (recur (map + [0 1] xy) :s (conj result [x y d]))
        (and (= d :w) (= t \\)) (recur (map + [0 -1] xy) :n (conj result [x y d]))
        (and (= d :e) (= t \\)) (recur (map + [0 1] xy) :s (conj result [x y d]))
        (and (= d :e) (= t \/)) (recur (map + [0 -1] xy) :n (conj result [x y d]))
        ;
        (and (= d :n) (= t \|)) (recur (map + [0 -1] xy) :n (conj result [x y d]))
        (and (= d :s) (= t \|)) (recur (map + [0 1] xy) :s (conj result [x y d]))
        (and (= d :w) (= t \-)) (recur (map + [-1 0] xy) :w (conj result [x y d]))
        (and (= d :e) (= t \-)) (recur (map + [1 0] xy) :e (conj result [x y d]))
        ;
        (and (= d :n) (= t \-)) (s/union 
          (energize (map + [-1 0] xy) :w (conj result [x y d]))
          (energize (map + [1 0] xy) :e (conj result [x y d])))
        (and (= d :s) (= t \-)) (s/union 
          (energize (map + [-1 0] xy) :w (conj result [x y d]))
          (energize (map + [1 0] xy) :e (conj result [x y d])))
        (and (= d :w) (= t \|)) (s/union 
          (energize (map + [0 -1] xy) :n (conj result [x y d]))
          (energize (map + [0 1] xy) :s (conj result [x y d])))
        (and (= d :e) (= t \|)) (s/union 
          (energize (map + [0 -1] xy) :n (conj result [x y d]))
          (energize (map + [0 1] xy) :s (conj result [x y d])))
        ;
        :else (println "unexpected event" x y d t))
    ))

(def energized (energize [0 0] :e #{}))
; (println energized)
(def energized-nodir (set (map (fn [xyd] (let [[x y d] xyd] [x y])) energized)))
; (println energized-nodir)
(println (count energized-nodir))

