(ns mod-p
  (:require [heavy-bool :refer [+and +forall +exists +false +true +conj +conj-true +conj-false heavy-bool?]]))


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
                (+conj +false {:a a
                               :b b
                               :reason "not equal"})))
            (mult [a b]
              (mod (* a b) p))
            (member [a]
              {:post [(heavy-bool? %)]}
              (+conj-false [(<= 0 a p) ()] {:reason "expecting 0 <= a < p"
                                            :a a
                                            :p p}))
            (invert [a]
              {:post [(heavy-bool? %)]}
              (+conj-false (+exists inv-a elements
                                    (+and (equiv ident (mult a inv-a))
                                          (equiv ident (mult inv-a a))))
                           {:reason "cannot compute inverse of"
                            :a a}))]
      {:p p
       :gen elements
       :equiv equiv
       :invert invert
       :member member
       :op mult
       :ident 1})))
