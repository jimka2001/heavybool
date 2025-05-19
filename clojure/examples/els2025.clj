(ns els2025
  "Examples to support els 2025 paper submission"
  (:require [heavy-bool :as hb]
            [util :refer [forall]])
)

(let [M [1 2 3 4 5]
      op (fn [x y] (- x y))]
  (some (fn [x]
          (some (fn [y] (when (not= (op x y) (op y x))
                          [x y]))
                M))
        M))

(let [M [1 2 3 4 5]
      op (fn [x y] (- x y))]
  (forall [a M
           b M]
    (= (op a b)
       (op b a))))


(let [M [1 2 3 4 5]
      op (fn [x y] (- x y))]
  (hb/+forall [a M
               b M]
    (= (op a b)
       (op b a))))

(hb/+exists [a (range 4 100)
             b (range (inc a) 100)
             c (range (inc b) 100)]
  (= (+ (* a a) (* b b))
     (* c c)))

(hb/+forall [a (range 4 100)
             b (range (inc a) 100)
             c (range (inc b) 100)]
  (= (+ (* a a) (* b b))
     (* c c)))

(defn is-symmetric [M <]
  (hb/+tag
   (hb/+forall [x M
                y M]
     (hb/+implies (< x y)
                  (< y x)))
   :symmetric))

(def M (range 1 20))

(is-symmetric M =)
(is-symmetric M <)

(defn is-transitive [M <]
  (hb/+tag
   (hb/+forall [x M
                y M]
     (hb/+implies (< x y)
                  (hb/+forall [z M]
                    (hb/+implies (< y z)
                                 (< x z)))))
   :transitive))

(is-transitive M =)
(is-transitive M <)
(is-transitive M not=)



