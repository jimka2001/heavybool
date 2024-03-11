(ns relations
    (:require [heavy-bool :refer [heavy-bool? +heavy-bool +not +if +true +exists +implies +and +forall +annotate-false +tag]]))


(defn is-reflexive [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag (+forall x gen
               (+annotate-false (+heavy-bool (rel x x))
                                :witness x))
             :reflexive))

(defn is-symmetric [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag 
   (+forall x gen
            (+forall y gen
                     (+annotate-false (+implies [(rel x y) ()]
                                                [(rel y x) ()])
                                      :x x
                                      :y y)))
   :symmetric))

(defn is-transitive
  "rel is a binary function which returns a Boolean"
  [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag
   (+forall x gen
     (+forall y gen
       (+implies (+heavy-bool (rel x y))
                 (+forall z gen
                   (+implies (+heavy-bool (rel y z))
                             (+heavy-bool (rel x z)))))))
   :transitive))

(defn is-equivalence [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag (+and (is-symmetric gen rel)
                   (is-reflexive gen rel)
                   (is-transitive gen rel))
             :equivalence))
  

(defn is-asymmetric [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag (+forall x gen
               (+forall y gen
                 (+annotate-false (+implies [(rel x y) ()]
                                        (+not [(rel y x) ()]))
                                  :x x
                                  :y y)))
             :assymetric))

(defn is-irreflexive [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag (+not (+exists x gen
                     [(rel x x) ()]))
             :irreflexive))
;; 
(defn is-strict-partial-order
  "A strict partial order is irreflexive, transitive, and asymmetric."
  [gen rel]
  {:pre [(sequential? gen)
         (fn? rel)]
   :post [(heavy-bool? %)]}
  (+tag (+and (is-irreflexive gen rel)
                   (is-transitive gen rel)
                   (is-asymmetric gen rel))
             :strict-partial-order))

