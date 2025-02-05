(ns els2025
  "Examples to support els 2025 paper submission"
  (:require [heavy-bool :as hb])
)

(defn example1 []
  (let [M [1 2 3 4 5]
        op (fn [x y] (- x y))]
    (some (fn [x]
            (some (fn [y] (when (not= (op x y) (op y x)) [x y]))
                  M))
          M)))

(hb/+exists [a (range 4 100)
             b (range a 100)
             c (range b 100)]
  (= (+ (* a a) (* b b))
     (* c c)))

(hb/+forall [a (range 4 100)
             b (range a 100)
             c (range b 100)]
  (= (+ (* a a) (* b b))
     (* c c)))
