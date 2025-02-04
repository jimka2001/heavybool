(ns magma-basic
  "Example usage of `heavy-bool`.
  This namespace implements tests for certain finite algebraic structures including:
  magma, semigroup, monoid, group, ring, and field.
  In each case we assume that the elements of the algebraic structures form a finite
  set.  Thus we may test the axioms, such as closure, associativity, and identity,
  using exhaustive seach.")

(defn is-closed
  [coll * member]
  (every? (fn [a]
            (every? (fn [b]
                      (member (* a b)))
                    coll))
          coll))
                               
(defn default-equal
  [left right]
  (= left right))

(defn is-associative
  [coll * equal]

  (every? (fn [a]
             (every? (fn [b]
                       (every? (fn [c]
                                 (equal (* a (* b c))
                                        (* (* a b) c)))
                               coll))
                     coll))
           coll))

(defn is-commutative
  [coll * equal]
  (every? (fn [a]
            (every? (fn [b]
                      (equal (* a b)
                             (* b a)))
                    coll))
          coll))

(defn is-identity
  [coll * ident equal]
  (every? (fn [a]
            (and (equal a (* ident a))
                 (equal a (* a ident))))
          coll))

(defn has-identity
  [coll * equal]
  (some (fn [e]
          (is-identity coll * e equal))
        coll))

(defn is-magma
  [coll * member]
  (is-closed coll * member))

(defn is-semigroup
  [coll * member equal]
  (and (is-magma coll * member)
       (is-associative coll * equal)))

(defn is-monoid
  [coll * ident member equal]
  (and (member ident)
       (is-semigroup coll * member equal)
       (is-identity coll * ident equal)))

(defn is-inverse [a * b ident equal]
  (and (equal ident (* b a))
       (equal ident (* a b))))

(defn has-inverses 
  [coll * ident member equal]
  (every? (fn [a]
            (some (fn [b]
                    (is-inverse a * b ident equal))
                  coll))
          coll))

(defn is-group 
  [coll * ident member equal]
  (and (is-monoid coll * ident member equal)
       (has-inverses coll * ident member equal)))


(defn is-ring
  [coll + * zero one member equal]
  (and (is-group coll + zero member equal)
       (is-commutative coll + equal)
       (is-monoid coll * one member equal)
       (every? (fn [a]
                 (every? (fn [b]
                           (every? (fn [c]
                                     (and (equal (* a (+ b c))
                                                 (+ (* a b) (* a c)))
                                          (equal (* (+ b c) a)
                                                 (+ (* b a) (* c a)))))
                                   coll))
                         coll))
               coll)))

(defn is-field
  [coll + *
   zero one
   member equal]
  (and (not (equal one zero))
       (is-ring coll + * zero one member equal)
       (is-commutative coll * equal)
       (every? (fn [x]
                 (or (equal x zero)
                     (some (fn [y]
                             (is-inverse x * y one equal))
                           coll)))
               coll)))
