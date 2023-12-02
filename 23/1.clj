(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input/1.txt")))

(defn firstlast [x]
  (let [digits (re-seq #"\d" x)
        nums (map read-string digits)]
    ; (println x digits nums)
    (+ (* 10 (first nums)) (last nums))))

(println (reduce + (map firstlast input)))

(def digits->nums {
  "1" 1
  "2" 2
  "3" 3
  "4" 4
  "5" 5
  "6" 6
  "7" 7
  "8" 8
  "9" 9
  "one" 1
  "two" 2
  "three" 3
  "four" 4
  "five" 5
  "six" 6
  "seven" 7
  "eight" 8
  "nine" 9
})

; so, the regex approach doesn't just work for text, in case of overlaps
; but we can fix it wwith the lookahead assertion

(defn firstlast2 [x]
  (let [digits (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" x)
        nums (map (fn [x] (get digits->nums (last x))) digits)]
    ; (println x digits nums)
    ; 0))
    (+ (* 10 (first nums)) (last nums))))

; (println (firstlast2 "howdyone334nine3123456789onetwothreefourfivesixsexveneightnine"))
(println (reduce + (map firstlast2 input)))

; try the test input to make sure
; the last line caused trouble!
(def input (str/split-lines "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
sixthree6lxcrsevenseven69twonegs
"))
(println (reduce + (map firstlast2 input)))
