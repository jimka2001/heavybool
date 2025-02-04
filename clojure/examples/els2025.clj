(ns els2025
  "Examples to support els 2025 paper submission"
  (:require [heavy-bool :as hb])
)

(def example1 []
  (let [M [1 2 3 4 5]
        op (fn [x y] (- x y))]
    (some (fn [x]
            (some (fn [y] (when (not= (op x y) (op y x)) [x y]))
                  M))
          M)))

(hb/+exists [a (range 1 100)
                b (range a 100)]
                  (hb/+forall [c (range b 100)]
                              (= 0 (* 0 (+ a b c)))))
