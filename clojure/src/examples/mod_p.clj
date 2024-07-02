(ns examples.mod-p
  "Example usage of `heavy-bool`.
  This namespace implements an algebraic structure `mod-p`.
  This structure is useful for testing the `examples.magma/is-group` function.
  "
  (:require [heavy-bool :refer [+and +forall +exists +false +true +annotate +annotate-true +annotate-false heavy-bool?]]))


(defn mod-p
  "The non-zero elements of the integers mod p (for prime p)
  is a group under multiplication."
  [p]
  {:pre [(int? p) (> p 1)]
   :post [(map? %)]}
  (let [elements (range 1 p)
        ident 1]
    (letfn [(equiv [a b]
              {:post [(heavy-bool? %)]}
              (if (= a b)
                +true
                (+annotate +false
                           :a a
                           :b b
                           :reason "not equal")))
            (mult [a b]
              (mod (* a b) p))
            (member [a]
              {:post [(heavy-bool? %)]}
              (+and (+annotate-false [(integer? a) ()] :reason "expecting integer, got a"
                                                    :a a)
                    (+annotate-false [(<= 0 a p) ()] :reason "expecting 0 <= a < p"
                                                  :a a
                                                  :p p)))
            (invert [a]
              {:post [(heavy-bool? %)]}
              (+annotate-false (+exists [inv-a elements]
                                    (+and (equiv ident (mult a inv-a))
                                          (equiv ident (mult inv-a a))))
                               :reason "cannot compute inverse of"
                               :a a))]
      {:p p
       :gen elements
       :equiv equiv
       :invert invert
       :member member
       :op mult
       :ident 1})))
