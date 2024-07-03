(ns mod-p
  "Example usage of `heavy-bool`.
  This namespace implements an algebraic structure `mod-p`.
  This structure is useful for testing the `examples.magma/is-group` function.
  "
  (:require [heavy-bool :refer [+and +forall +exists +false +true +annotate +annotate-true +annotate-false
                                +heavy-bool heavy-bool?]]))


(defn mod-p
  "The non-zero elements of the integers mod p (for prime p)
  is a group under multiplication.
  This function returns a map with the keys:
    :p  -- positive integer for which the operation is performed modulo p
    :gen -- a collection of integers from 1 to p-1
    :equiv -- a heavy-boolean relation (binary function returning a `heavy-bool`)
    :invertible -- a predicate returning a `heavy-bool` indicating whether a given element is invertible.
               If it is invertible, the return value
    :member -- a membership predicate returning a `heavy-bool`
    :op -- integer multiplication mod p
    :ident -- 1
  "
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
              (+and (+annotate-false (+heavy-bool (integer? a) :a a :p p)
                                     :reason "expecting integer, got a")
                    (+annotate-false (+heavy-bool (<= 0 a p) :a a :p p)
                                     :reason "expecting 0 <= a < p")))
            (invertible [a]
              {:post [(heavy-bool? %)]}
              (+annotate-false (+exists [inv-a elements]
                                        (+and (equiv ident (mult a inv-a))
                                              (equiv ident (mult inv-a a))))
                               :reason "cannot compute inverse of"
                               :a a))]
      {:p p
       :gen elements
       :equiv equiv
       :invertible invertible
       :member member
       :op mult
       :ident 1})))
