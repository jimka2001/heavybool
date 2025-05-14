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

(def words 
  (with-open [rdr (clojure.java.io/reader "/usr/share/dict/words")]
    (doall (line-seq rdr))))


(defn alpha-< [a b]
  (cond (= a b)
        false

        (empty? a)
        true

        (empty? b)
        false

        

        (not= (first a) (first b))
        (neg? (compare (first a) (first b)))

        :else
        (alpha-< (rest a) (rest b))))


(alpha-< '(a) '(b a))


(hb/+forall [a words
             b words
             c words]
  (hb/+implies (and (alpha-< a b)
                    (alpha-< b c))
               (alpha-< a c)))
