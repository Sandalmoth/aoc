(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/2.txt")))

(defn game-num [x]
  (read-string (last (re-find #"(\d+):" x))))

; (println (map game-num input))

(defn round-possible? [x]
  (let [r (re-find #"(\d+) red" x)
        g (re-find #"(\d+) green" x)  
        b (re-find #"(\d+) blue" x)]
    ; (println r g b)
    (and 
      (if (nil? r) true (< (read-string (last r)) 13))
      (if (nil? g) true (< (read-string (last g)) 14))
      (if (nil? b) true (< (read-string (last b)) 15))
    )))

; (println (round-possible? "10 red, 1 green, 13 blue"))
; (println (round-possible? "1 green, 15 blue"))

(defn game-possible? [x]
  (let [all-rounds (last (str/split x #":"))
        rounds (str/split all-rounds #";")
        rounds-possible (map round-possible? rounds)]
    ; (println rounds-possible)
    (every? true? rounds-possible)))

; (println (game-possible? "Game 60: 2 green, 1 blue; 1 green, 1 blue, 4 red; 3 blue, 1 red, 1 green; 2 red, 2 green; 4 red"))

; (println (filter game-possible? input))
; (println (map game-num (filter game-possible? input)))
(println (reduce + (map game-num (filter game-possible? input))))

(defn round-mins [x]
  (let [r (re-find #"(\d+) red" x)
        g (re-find #"(\d+) green" x)  
        b (re-find #"(\d+) blue" x)]
    ; (println r g b)
    [
      (if (nil? r) 0 (read-string (last r)))
      (if (nil? g) 0 (read-string (last g)))
      (if (nil? b) 0 (read-string (last b)))
    ]))

; (println (round-mins "10 red, 1 green, 13 blue"))
; (println (round-mins "1 green, 15 blue"))

(defn game-power [x]
  (let [all-rounds (last (str/split x #":"))
        rounds (str/split all-rounds #";")
        round-mins (map round-mins rounds)
        r (apply max (map (fn [y] (get y 0)) round-mins))
        g (apply max (map (fn [y] (get y 1)) round-mins))
        b (apply max (map (fn [y] (get y 2)) round-mins))]
    ; (println round-mins r g b)
    (* r g b)))

; (println (game-power "Game 60: 2 green, 1 blue; 1 green, 1 blue, 4 red; 3 blue, 1 red, 1 green; 2 red, 2 green; 4 red"))

(println (reduce + (map game-power input)))

