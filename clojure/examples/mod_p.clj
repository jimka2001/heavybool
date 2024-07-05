(ns mod-p
  "Example usage of `heavy-bool`.
  This namespace implements an algebraic structure `mod-p`.
  This structure is useful for testing the `examples.magma/is-group` function.
  "
  (:require [heavy-bool :refer [+and +forall +exists +false +true +annotate +annotate-true +annotate-false
                                +tag +heavy-bool heavy-bool?]]
            [util :as ut]
))


(defn- mod-p
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
  [p elements op ident]
  {:pre [(int? p) (> p 1)]
   :post [(map? %)]}
  (letfn [(equiv [a b]
            (if (= a b)
              +true
              (+annotate +false
                         :a a
                         :b b
                         :reason "not equal")))
          (member [a]
            (+tag (+annotate (ut/member a elements) :a a :p p)
                  :member))
          (invertible [a]
            (+annotate-false (+exists [inv-a elements]
                                      (+and (equiv ident (op a inv-a))
                                            (equiv ident (op inv-a a))))
                             :reason "cannot compute inverse of"
                             :a a))]
    {:p p
     :gen elements
     :equiv equiv
     :invertible invertible
     :member member
     :op op
     :ident ident}))


(defn addition-mod-p
  [p]
  (mod-p p
         (range p)
         (fn [a b] (mod (+ a b) p))
         0))

(defn multiplication-mod-p
  [p]
  (mod-p p
         (range 1 p)
         (fn [a b] (mod (* a b) p))
         1))
