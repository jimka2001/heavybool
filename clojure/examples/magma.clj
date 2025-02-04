(ns magma
  "Example usage of `heavy-bool`.
  This namespace implements tests for certain finite algebraic structures including:
  magma, semigroup, monoid, group, ring, and field.
  In each case we assume that the elements of the algebraic structures form a finite
  set.  Thus we may test the axioms, such as closure, associativity, and identity,
  using exhaustive seach."
  (:require [heavy-bool :refer [+bool +and +or +not +forall +exists +annotate
                                +conj +annotate-false heavy-bool? +tag find-witness]]))

(defn is-closed
  "Predicate returning a `heavy-bool`, determining whether the given operation `*`
  is closed on the given collection `coll`.
  `coll` -- a collection of values representing a finite mathematical set.
  `*` -- a binary operator which accepts two elements of `coll`
  `member` -- membership predicate returning a `heavy-bool`"
  [coll * member]
  {:pre [(fn? *)
         (fn? member)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll
             b coll]
            (member (* a b)))
   :closed))
                               
(defn default-equal
  "The default equal predicate which returns a `heavy-bool` indicating
  whether the arguments `left` and `right` are equal to each other"
  [left right]
  {:post [(heavy-bool? %)]}
  (+tag
   (+annotate-false (= left right)
                    :left left
                    :right right)
   :equal))

(defn is-associative
  "Predicate returning a `heavy-bool` indicating whether the given operation
  is associative on the given collection of elements.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `equal` -- equivalence predicate returning a `heavy-bool`"
  [coll * equal]
  {:pre [(fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll
             b coll
             c coll]
            (equal (* a (* b c))
                   (* (* a b) c)))
   :associative))

(defn is-commutative
  "Predicate returning a `heavy-bool` indicating whether the given operation
  is commutative on the given collection of elements.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `equal` -- equivalence predicate returning a `heavy-bool`"
  [coll * equal]
  {:pre [(fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll
             b coll]
            (equal (* a b)
                   (* b a)))
   :commutative))

(defn is-identity
  "Predicate returning a `heavy-bool` indicating whether the proposed identity, `ident`
  is really a left and right identity on the given collection of elements.
   `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `ident` -- the proposed identity element
   `equal` -- equivalence predicate returning a `heavy-bool`"
  [coll * ident equal]
  {:pre [(fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+annotate-false
   (+forall [a coll]
            (+and (equal (* ident a) a)
                  (equal (* a ident) a)))
   :identity false
   :ident ident))

(defn find-identity
  "Predicate returning `heavy-bool` indicating whether there exists an identity element
  in the given collection of elements.   If such an element is found, it may be extracted
  from the `heavy-bool` using `find-witness`.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `equal` -- equivalence predicate returning a `heavy-bool`"
  [coll * equal]
  {:pre [(fn? *)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+exists [e coll]
    (is-identity coll * e equal)))



(defn is-magma
  "Predicate returning a `heavy-bool` indicating whether the given collection
   forms a magma under the given operation.
    A magma is a set which is closed under the operation
   `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `member` -- membership predicate returning a `heavy-bool`"
  [coll * member]
  {:pre [(fn? *)
         (fn? member)]
   :post [(heavy-bool? %)]}
  (+tag  (is-closed coll * member)
         :magma))

(defn is-semigroup
  "Predicate returning a `heavy-bool` indicating whether the given collection
   forms a semigroup under the given operation.
   A semigroup is a magma with an associative operation.
   `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `member` -- membership predicate returning a `heavy-bool`
   `equal` -- equivalence predicate returning a `heavy-bool`"
  [coll * member equal]
  {:pre [(fn? *)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag  (+and (is-magma coll * member)
               (is-associative coll * equal))
         :semigroup))

(defn is-monoid
  "Predicate returning a `heavy-bool` indicating whether the given collection
   forms a monoid under the given operation.
   A monoid is a semigroup containing an identity for the given operation.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `member` -- membership predicate returning a `heavy-bool`
   `equal` -- equivalence predicate returning a `heavy-bool`"
  [coll * ident member equal]
  {:pre [(fn? *)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+and (is-semigroup coll * member equal)
         (is-identity coll * ident equal)
         )
   :monoid))

(defn has-inverses 
  "Predicate returning a `heavy-bool` to determine whether every element
  of `coll` has an inverse for the given `*` operation w.r.t. the given `ident`.
    `coll` -- collections of object in the magma
    `*` -- binary function, takes two elements from `coll` and returns a new element
    `ident` -- the identity for the magma
    `invertible` -- function which takes an element from `coll` and returns a heavy-bool
                  indicating whether the given element is invertible.  If an element
                  is invertible, then `(find-witness hb)` should returns its inverse.
    `member` -- membership predicate, returning `heavy-bool`
    `equal` -- equality predicate, returning `heavy-bool`.
  "
  [coll * ident invertible member equal]
  {:pre [(fn? *)
         (fn? invertible)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [a coll
             :let [inv-a (invertible a)
                   b (find-witness inv-a)]]
     (+annotate (+tag
                 (+and inv-a
                       (member b)
                       (equal (* b a) ident)
                       (equal (* a b) ident))
                 :invertible)
                :inv-a inv-a))
   :has-inverses))

(defn is-group 
  "Predicate returning a `heavy-bool` indicating whether the given `coll` is a group
  under the given operation `*` with identity `ident`.
  A group is a monoid which has inverses for all elements.
    `coll` -- collections of object in the magma
    `*` -- binary function, takes two elements from `coll` and returns a new element
    `ident` -- the identity for the magma
    `invertible` -- function which takes an element from `coll` and returns a heavy-bool
                  indicating whether the given element is invertible.  If an element
                  is invertible, then `(find-witness hb)` should returns its inverse.
    `member` -- membership predicate, returning `heavy-bool`
    `equal` -- equality predicate, returning `heavy-bool`.
  "
  [coll * ident invertible member equal]
  {:pre [(fn? *)
         (fn? invertible)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+and (is-monoid coll * ident member equal)
         (has-inverses coll * ident invertible member equal))
   :group))


(defn is-ring
  "Predicate returning a `heavy-bool` indicating whether the given collection
   forms a ring under the given operation.
   A ring is a collection of elements which forms a commutative group under the addition operation,
   and forms a monoid under the multiplication operation.
  `coll` -- a collection of values representing a finite mathematical set.
   `+` -- binary addition function, takes two elements from `coll` and returns a new element
   `*` -- binary multiplication function, takes two elements from `coll` and returns a new element
  `zero` -- the proposed identity for addition
  `one` -- the proposed identity for multiplication
  `+invertible` -- a predicate returning a `heavy-bool` which takes an element of `coll` and
          returns a `heavy-bool` indicating whether that element is invertible under addition.
          If the `heavy-bool` is heavy-true, then `find-witness` can be called on the `heavy-bool`
          to extract the inverse.
  `member` -- membership predicate, returning `heavy-bool`
  `equal` -- equality predicate, returning `heavy-bool`."
  [coll + * zero one +invertible member equal]
  {:pre [(fn? +)
         (fn? *)
         (fn? +invertible)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+and (is-group coll + zero +invertible member equal)
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

(defn is-field
  "Predicate returning a `heavy-bool` indicating whether the given collection
   forms a field under the given operations.
   A field is a collection of elements along with two commutative operations, addition and multiplication,
   which forms a ring under the two operations,
   and for which the additive and multiplicative identities are distinct (not equal),
   and for which all the non-zero elements have multiplicative inverses.
   `coll` -- a collection of values representing a finite mathematical set.
   `+` -- binary addition function, takes two elements from `coll` and returns a new element
   `*` -- binary multiplication function, takes two elements from `coll` and returns a new element
  `zero` -- the proposed identity for addition
  `one` -- the proposed identity for multiplication
  `+invertible` -- a predicate returning a `heavy-bool` which takes an element of `coll` and
          returns a `heavy-bool` indicating whether that element is invertible under addition.
          If the `heavy-bool` is heavy-true, then `find-witness` can be called on the `heavy-bool`
          to extract the inverse.
  `*invertible` -- a predicate returning a `heavy-bool` which takes an element of `coll` and
          returns a `heavy-bool` indicating whether that element is invertible under multiplication.
          If the `heavy-bool` is heavy-true, then `find-witness` can be called on the `heavy-bool`
          to extract the inverse.
  `member` -- membership predicate, returning `heavy-bool`
  `equal` -- equality predicate, returning `heavy-bool`."
  [coll + *
   zero one
   +invertible *invertible
   member equal]
  {:pre [(fn? +)
         (fn? *)
         (fn? +invertible)
         (fn? *invertible)
         (fn? member)
         (fn? equal)]
   :post [(heavy-bool? %)]}
  (+tag
   (+and (+not (equal one zero))
         (+annotate-false (is-ring coll + * zero one +invertible member equal)
                          :zero zero
                          :one one)
         (is-commutative coll * equal)
         (+forall [x coll
                   :when (+not (equal x zero))
                   :let [inv (*invertible x)]]
                  (+tag (+and inv
                              (member (find-witness inv)))
                        :invertible)))
   :field))
        
