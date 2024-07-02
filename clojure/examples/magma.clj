(ns magma
  "Example usage of `heavy-bool`.
  This namespace implements tests for certain finite algebraic structures including:
  magma, semigroup, monoid, group, ring, and field.
  In each case we assume that the elements of the algebraic structures form a finite
  set.  Thus we may test the axioms, such as closure, associativity, and identity,
  using exhaustive seach."
  (:require [util :refer [type-check first-st]]
            [heavy-bool :refer [+bool +and +or +not +forall +exists +annotate +conj +annotate-false heavy-bool? +tag]]))

(defn is-closed [coll * member]
  {:pre [(seq? coll)
         (fn? *)
         (fn? member)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll]
     (+forall [b coll]
       (member (* a b))))
   :closed))
                               
(defn default-equal [left right]
  {:post [(heavy-bool? %)]}
  (+tag
   (+annotate-false [(= left right) ()]
                    :left left
                    :right right)
   :equal))

(defn is-associative [coll * equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll
             b coll
             c coll]
            (equal (* a (* b c))
                   (* (* a b) c)))
   :associative))

(defn is-commutative [coll * equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll
             b coll]
            (equal (* a b)
                   (* b a)))
   :commutative))

(defn is-identity [coll * ident equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll]
     (+annotate-false (equal (* ident a)
                         (* a ident))
                      :ident ident))
   :identity))

(defn find-identity [coll * equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+exists [e coll ]
    (is-identity coll * e equal)))

(defn is-semigroup [coll * member equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag  (+and (is-closed coll * member)
                    (is-associative coll * equal))
              :semigroup))

(defn is-monoid [coll * ident member equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+and (member ident)
         (is-semigroup coll * member equal)
         (is-identity coll * ident equal))
   :monoid))

(defn has-inverses [coll * ident invert member equal]
  {:pre [(seq? coll)
         (fn? *)
         (fn? invert)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll]
            (let [[_ reasons :as inv-a] (invert a)
                  b (:witness (first-st r reasons (:witness r)))]
              (+annotate (+tag
                          (+and inv-a
                                (member b)
                                (equal (* b a) ident)
                                (equal (* a b) ident))
                          :invertable)
                         :reasons reasons
                         :inv-a inv-a)
              ))
   :has-inverses))

(defn is-group [coll * ident invert member equal]
  {:pre [(fn? *)
         (fn? invert)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+and (is-monoid coll * ident member equal)
         (has-inverses coll * ident invert member equal))
   :group))

(defn is-ring [coll + * zero one +inv member equal]
  {:pre [(seq? coll)
         (fn? +)
         (fn? *)
         (fn? +inv)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+and (is-group coll + zero +inv member equal)
        (is-commutative coll + equal)
        (is-monoid coll * one member equal)
        (+forall [a coll
                  b coll
                  c coll]
              (+and (+tag (equal (* a (+ b c))
                                      (+ (* a b) (* a c)))
                               :left-distributive)
                    (+tag (equal (* (+ b c) a)
                                      (+ (* b a) (* c a)))
                               :right-distributive)))))

(defn is-field [coll + *
                zero one
                +inv *inv
                member equal]
  {:pre [(seq? coll)
         (fn? +)
         (fn? *)
         (fn? +inv)
         (fn? *inv)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+and (+not (equal one zero))
        (+annotate-false (is-ring coll + * zero one +inv member equal)
                         :zero zero
                         :one one)
        (is-commutative coll * equal)
        (+forall [x coll]
          (+or (equal x zero)
               (let [[_ reason :as maybe-inv] (*inv x)]
                 (+tag
                  (+and maybe-inv
                        (member (:witness (first reason))))
                  :invertable))))
        ))
        
