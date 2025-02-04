(ns magma-basic-2
  "This namespace implements tests for certain finite algebraic structures including:
  magma, semigroup, monoid, group, ring, and field.
  In each case we assume that the elements of the algebraic structures form a finite
  set.  Thus we may test the axioms, such as closure, associativity, and identity,
  using exhaustive seach."
  (:require [util :refer [forall exists]]))

(defn is-closed
  [coll * member]
  (forall [a coll
           b coll]
    (member (* a b))))
                               
(defn is-associative
  [coll * =]
  (forall [a coll
           b coll
           c coll]
    (= (* a (* b c))
       (* (* a b) c))))

(defn is-commutative
  [coll * =]
  (forall [a coll
           b coll]
    (= (* a b)
       (* b a))))

(defn is-identity
  [coll * ident =]
  (forall [a coll]
    (and (= a (* ident a))
         (= a (* a ident)))))

(defn has-identity
  [coll * =]
  (exists [e coll]
    (is-identity coll * e =)))

(defn is-magma
  [coll * member]
  (is-closed coll * member))

(defn is-semigroup
  [coll * member =]
  (and (is-magma coll * member)
       (is-associative coll * =)))

(defn is-monoid
  [coll * ident member =]
  (and (member ident)
       (is-semigroup coll * member =)
       (is-identity coll * ident =)))

(defn is-inverse [a * b ident =]
  (and (= ident (* b a))
       (= ident (* a b))))

(defn has-inverses 
  [coll * ident =]
  (forall [a coll]
    (exists [b coll]
      (is-inverse a * b ident =))))

(defn is-group 
  [coll * ident member =]
  (and (is-monoid coll * ident member =)
       (has-inverses coll * ident =)))


(defn is-ring
  [coll + * zero one member =]
  (and (is-group coll + zero member =)
       (is-commutative coll + =)
       (is-monoid coll * one member =)
       (forall [a coll
                b coll
                c coll]
         (and (= (* a (+ b c))
                 (+ (* a b) (* a c)))
              (= (* (+ b c) a)
                 (+ (* b a) (* c a)))))))

(defn is-field
  [coll + *
   zero one
   member =]
  (and (not (= one zero))
       (is-ring coll + * zero one member =)
       (is-commutative coll * =)
       (forall [x coll]
         (or (= x zero)
             (exists [y coll]
               (is-inverse x * y one =))))))
