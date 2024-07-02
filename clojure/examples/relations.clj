(ns relations
  "Example usage of `heavy-bool`.
  This namespace defines several relations such as reflexive, symmetric, and antisymmetric.
  "
  (:require [heavy-bool :refer [heavy-bool? +heavy-bool +not +if +true +exists +implies
                                +and +or +forall +annotate-false +tag]]))

(defn is-reflexive 
  "A reflexive relation is a relation for (a R a) always
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+forall [x gen]
          (hb-rel x x))
        :reflexive))

(defn is-symmetric 
  "A symmetric relation means that (a R b) => (b R a).
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag 
   (+forall [x gen
             y gen]
     (+implies (hb-rel x y)
               (hb-rel y x)))
   :symmetric))

(defn is-transitive
  "hb-rel is a binary function which returns a `heavy-bool`
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall [x gen
             y gen]
     (+implies (hb-rel x y)
               (+forall [z gen]
                 (+implies (hb-rel y z)
                           (hb-rel x z)))))
   :transitive))

(defn is-equivalence 
  "An equivalence relation is symmetric, reflexive, and transitive.
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+and (is-symmetric gen hb-rel)
              (is-reflexive gen hb-rel)
              (is-transitive gen hb-rel))
        :equivalence))
  

(defn is-asymmetric 
  "Test for asymmetric relation. (a R b) => (not (b R a))
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+forall [x gen
                  y gen]
          (+implies (hb-rel x y)
                    (+not (hb-rel y x))))
        :assymetric))

(defn is-antisymmetric 
  "Test for antisymmetric relation:
  ((a R b) and (b R a)) => (a = b)
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]
   }
  (+tag (+forall [a gen
                  b gen]
          (+implies (+and (hb-rel a b)
                          (hb-rel b a))
                    (+heavy-bool (= a b))))
     :antisymmetric))

(defn is-irreflexive
  "An irreflexive relation is a relation for which (x R x) is always false
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+not (+exists [x gen]
                (hb-rel x x)))
        :irreflexive))

(defn is-partial-order 
  "A partial order (or weak partial order) relation is
  reflexive, antisymmetric, and transitive.
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool."
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+and (is-reflexive gen hb-rel)
              (is-antisymmetric gen hb-rel)
              (is-transitive gen hb-rel))
        :partial-order))

(defn is-strict-partial-order
  "A strict partial order is irreflexive, transitive, and asymmetric.
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+and (is-irreflexive gen hb-rel)
              (is-transitive gen hb-rel)
              (is-asymmetric gen hb-rel))
        :strict-partial-order))

(defn is-connected
  "A connected relation means that if x!=y then (x R y) or (y R x)
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+forall [a gen
                  b gen]
          (+implies (+heavy-bool (not= a b))
                    (+or (hb-rel a b)
                         (hb-rel b a))))
     :connected))

(defn is-strongly-connected
  "A strongly connected relation means that (x R y) or (y R x)
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool"
  [gen hb-rel]
  {:pre [(sequential? gen)
         (fn? hb-rel)]
   :post [(heavy-bool? %)]}
  (+tag (+forall [a gen
                  b gen]
          (+or (hb-rel a b)
               (hb-rel b a)))
     :strongly-connected))

(defn lift-relation
  "Accepts a relation, a binary predicate returning a boolean,
  and returns a binary predicate returning a heavy-boolean."
  [rel]
  (fn [a b]
    (+heavy-bool (rel a b))))

