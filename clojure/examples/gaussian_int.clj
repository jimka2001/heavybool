(ns gaussian-int
  "Example usage of `heavy-bool`.
  This namespace implements Gaussian Integers [Gaussian integer](https://en.wikipedia.org/wiki/Gaussian_integer)
  but restricted to integers modulo some prime, p."
  (:require [util :refer [find-if]]
            [heavy-bool :refer [+and +forall +exists +false +true +annotate +conj +tag heavy-bool?]]))

(defn gaussian? [g]
  (and (vector? g)
       (= 2 (count g))
       (int? (first g))
       (int? (second g))))

(defn gaussian-int-mod-p [p]
  {:pre [(int? p)
         (< 1 p)]
   :post [(map? %)
          (seq? (:gen %))]}
  (let [zero [0 0]
        one [1 0]]
    (letfn [(gmod ([a]
                   {:pre [(int? a)]
                    :post [(int? %)
                           (<= 0 % (dec p))]}
                   (mod a p))
              ([a b]
               {:pre [(int? a)
                      (int? a)]
                :post [(gaussian? %)]}
               [(gmod a) (gmod b)]))
            (add [[x y :as a]
                  [u v :as b]]
              {:pre [(gaussian? a)
                     (gaussian? b)]
               :post [(gaussian? %)]}
              (gmod (+ x u) (+ y v)))
            (subtract [[x y :as a]
                       [u v :as b]]
              {:pre [(gaussian? a)
                     (gaussian? b)]
               :post [(gaussian? %)]}
              (gmod (- x u) (- y v)))
            (add-inv [a]
              {:pre [(gaussian? a)]
               :post [(heavy-bool? %)]}
              (+annotate +true :witness (subtract zero a)
                         :zero zero
                         :a a))
            (mult [[x y :as a]
                   [u v :as b]]
              {:pre [(gaussian? a)
                     (gaussian? b)]
               :post [(gaussian? %)]}
              (gmod (- (* u x) (* v y))
                    (+ (* u y) (* v x))))
            (mult-inv [[a b :as ab]]
              {:pre [(gaussian? ab)]
               :post [(heavy-bool? %)]}
              (let [denom (gmod (+ (* a a)
                                   (* b b)))]
                (if (= 0 denom)
                  (+annotate +false
                             :reason "gaussian integer ab not invertible mod p"
                             :p p
                             :ab ab)
                  (let [[z :as found] (find-if (fn [z] (= 1 (gmod (* z denom))))
                                               (range 1 (inc p)))]
                    (if found
                      (+annotate +true
                                 :witness (gmod (* a z) (- (* b z)))
                                 :z z)
                      (+annotate +false
                                 :reason "gaussian integer ab not invertible mod p"
                                 :p p
                                 :ab ab))))))
            (op [a b]
              {:pre [(gaussian? a)
                     (gaussian? b)]
               :post [(gaussian? %)]}
              (add a b))
            (gen []
              ;; return lazy list of gaussian numbers
              (for [u (range 0 p)
                    v (range 0 p)]
                [u v]))
            (equiv [a b]
              {:pre [(gaussian? a)
                     (gaussian? b)]
               :post [(heavy-bool? %)]}
              (+tag
               [(= a b)
                (list {:a a :b b})]
               :equal))
            (member [[x y :as a]]
              {:pre [x
                     y
                     (int? x)
                     (int? y)
                     (gaussian? a)]
               :post [(heavy-bool? %)]}
              (+tag
               (+conj 
                (cond (< x 0)
                      (+annotate +false :reason "x < 0")
                      (< y 0)
                      (+annotate +false :reason "y < 0")
                      (>= x p)
                      (+annotate +false :reason "x >= p")
                      (>= y p)
                      (+annotate +false :reason "y >= p")
                      :else
                      +true)
                {:p p :x :y})
               :member))]
      {:p p
       :gen (gen)
       :op op
       :equiv equiv
       :mult mult
       :one one
       :zero zero
       :add add
       :add-inv add-inv
       :mult-inv mult-inv
       :member member})))
