(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/5.txt")))
; (def maxnum (apply max (map parse-long (re-seq #"\d+" (slurp "input/5.txt")))))

; (println maxnum)

(def seeds (map parse-long (re-seq #"\d+" (first input))))

; (println seeds)

(defn is-numbers [l]
  (let [m (re-find #"^\d+ \d+ \d+" l)]
    ; (println m)
    (nil? m)))

(def map-data (filter #(not (is-numbers (first %))) (partition-by is-numbers input)))

; (mapv println map-data)

; not actually a map, the data is really big
(defn make-map [data]
  (let [parse-nums (fn [l] (map parse-long (re-seq #"\d+" l)))
        nums (map parse-nums data)]
    ; (println nums)
    nums))
    ; (reduce (fn [coll x] (let [[dest src len] x] 
    ;   (assoc coll src [dest len])
    ;   )
    ;   ; (assoc coll x :yo)
    ;   ) {} nums)
    ; ))


; (println (make-map (first map-data)))

(defn remap [x m]
  (if (empty? m)
    x
    (let [[dst src len] (first m)
          end (+ src len)]
      (if (and (>= x src) (< x end))
        (+ dst (- x src))
        (recur x (rest m))
        ))))

; (println (remap 3640772818 (make-map (first map-data))))

; (println (reduce remap (first seeds) (map make-map map-data)))

(defn do-remap [x] (reduce remap x (map make-map map-data)))

(println (apply min (map do-remap seeds)))

; we could get the answer like this in theory
; however, the seed ranges are too big and it would take forever

(def seed-ranges (reduce concat (map (fn [[start len]] 
  (range start (+ start len))
  ) (partition 2 seeds))))

(println (apply min (map do-remap seed-ranges)))

