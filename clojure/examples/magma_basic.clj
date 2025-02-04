(ns magma-basic
  "This namespace implements tests for certain finite algebraic structures including:
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
                               
(defn is-associative
  [coll * =]
  (every? (fn [a]
             (every? (fn [b]
                       (every? (fn [c]
                                 (= (* a (* b c))
                                        (* (* a b) c)))
                               coll))
                     coll))
          coll))

(defn is-commutative
  [coll * =]
  (every? (fn [a]
            (every? (fn [b]
                      (= (* a b)
                             (* b a)))
                    coll))
          coll))

(defn is-identity
  [coll * ident =]
  (every? (fn [a]
            (and (= a (* ident a))
                 (= a (* a ident))))
          coll))

(defn has-identity
  [coll * =]
  (some (fn [e]
          (is-identity coll * e =))
        coll))

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
  (every? (fn [a]
            (some (fn [b]
                    (is-inverse a * b ident =))
                  coll))
          coll))

(defn is-group 
  [coll * ident member =]
  (and (is-monoid coll * ident member =)
       (has-inverses coll * ident =)))


(defn is-ring
  [coll + * zero one member =]
  (and (is-group coll + zero member =)
       (is-commutative coll + =)
       (is-monoid coll * one member =)
       (every? (fn [a]
                 (every? (fn [b]
                           (every? (fn [c]
                                     (and (= (* a (+ b c))
                                             (+ (* a b) (* a c)))
                                          (= (* (+ b c) a)
                                             (+ (* b a) (* c a)))))
                                   coll))
                         coll))
               coll)))

(defn is-field
  [coll + *
   zero one
   member =]
  (and (not (= one zero))
       (is-ring coll + * zero one member =)
       (is-commutative coll * =)
       (every? (fn [x]
                 (or (= x zero)
                     (some (fn [y]
                             (is-inverse x * y one =))
                           coll)))
               coll)))
