(ns heavy-bool
  "A heavy-bool is a pair [bool reason], where bool is a truth value
  usually true or false, but may be any clojure truthy or falsy value.
  reason is a list of maps with keys such as :witness, :bool, and
  :predicate etc.  A heavy-bool answers a predicate question with either
  yes-because or no-because")


(def +true [true ()])
(def +false [false ()])

(defn heavy-bool? [heavy-bool]
  (and (vector? heavy-bool)
       (not-empty heavy-bool)
       (= 2 (count heavy-bool))
       (list? (second heavy-bool))
       (every? map? (second heavy-bool))))


(defn +lift
  "takes a boolean predicate and promotes to a predicate returning a heavy-bool
  with reason providing a witness value"
  [f]
  (fn [x] [(f x) (list {:witness x
                        :predicate f})]))

(defn +not [[bool reason]]
  [(not bool) reason])

(defn +heavy-bool [hb]
  (if (heavy-bool? hb)
    hb
    [hb ()]))

(defn +bool "convert heavy-bool to bool" [[bool reason]]
  bool)

(defmacro +if
  "heavy-bool version of `if`.  The condition must
  evaluate to a heavy-bool.  Either the consequent or
  alternative will be evaluated depending on the heavy-bool
  value."
  [cond consequent alternative]
  `(if (+bool ~cond)
     ~consequent
     ~alternative))

(defmacro +and
  "Exapands to code which evaluates to the left-most heavy-bool value
  in the argument list, otherwise evaluates to the right-most
  value.  If the argument list is empty, evaluates explicitly to
  +true"
  [& rest]
  (case (count rest)
    (0) +true
    (1) (first rest)
    (let [v (gensym)
          [head & tail] rest]
      `(let [~v ~head]
         (+if ~v
              (+and ~@tail)
              ~v)))))

(defmacro +or
  "Exapands to code which evaluates to the left-most heavy-bool value
  in the argument list, otherwise evaluates to the left-most
  value.  If the argument list is empty, evaluates explicitly to
  +false"
  [& rest]
  (case (count rest)
    (0) +false
    (1) (first rest)
    (let [v (gensym)
          [head & tail] rest]
      `(let [~v ~head]
         (+if ~v
              ~v
              (+or ~@tail))))))

(defmacro +implies
  [a b]
  `(+or (+not ~a)
       ~b))

(defmacro +implied-by
  [b a]
  `(+or ~b
        (+not ~a)))

(defn +conj
  "Conjoin an additional item to the reason list"
  [hb item]
  {:pre [(heavy-bool? hb)
         (map? item)]
   :post [(heavy-bool? %)]}
  (let [[bool reason] hb]
    [bool (conj reason item)]))

(defn +conj-true [heavy-bool reason]
  {:pre [(heavy-bool? heavy-bool)
         (map? reason)]
   :post [(heavy-bool? %)]}
  (+and heavy-bool
       (+conj heavy-bool reason)))

(defn +conj-false [heavy-bool reason]
  {:pre [(heavy-bool? heavy-bool)
         (map? reason)]
   :post [(heavy-bool? %)]}
  (+or heavy-bool
       (+conj heavy-bool reason)))

(defn +conj-if [heavy-bool true-reason false-reason]
  {:pre [(heavy-bool? heavy-bool)
         (map? true-reason)
         (map? false-reason)]
   :post [(heavy-bool? %)]}
  (+if heavy-bool
       (+conj-true heavy-bool true-reason)
       (+conj-false heavy-bool false-reason)))

(defn +annotate
  "Conjoin the given key paired with the boolean value of the given heavy-bool"
  [heavy-bool key]
  {:pre [(heavy-bool? heavy-bool)
         (keyword? key)]
   :post [(heavy-bool? %)]}
  (+conj heavy-bool {:key (+bool heavy-bool)}))

(defn +forall-
  "Functional version of +forall.
  Traverses the given collection until 1) either an item is found
  which is heavy-false and returned (with a new reason conjoined),
  or 2) else +true is returned.
  If some value in the collection causes the predicate to return
  heavy-false, then a reason will be specified which provides
  the :witness value (the counter-example) which caused the predicate
  to fail.  The :predicate is also given in the reason."
  [tag f coll]
  {:pre [(fn? f)
         (sequential? coll)]
   :post [(heavy-bool? %)]}
  (+conj 
   (reduce (fn [hb item]
             (assert (heavy-bool? hb))
             (let [this (+heavy-bool (f item))]
               (+if this
                    hb
                    (reduced (+conj this {:witness item
                                          })))))
           +true
           coll)
   {:tag tag}))

(defn +exists- 
  "Function version of +exists.
  Traverses the given collection until 1) either an item is found
  which is heavy-true and returned (with a new reason conjoined),
  or 2) else returns explicitly +false.
  If some value in the collection causes the predicate to return
  heavy-true, then a reason will be specified which provides
  the :witness value (the example) which caused the predicate
  to succeed.  The :predicate is also given in the reason."
  [tag f coll]
  {:pre [(fn? f)
         (sequential? coll)]
   :post [(heavy-bool? %)]}
  (+not (+forall- tag (fn [x] (+not (f x))) coll)))

(defmacro +exists
  "Existential quantifier syntax.  body is expected to evaluate
  to a heavy-bool"
  [var coll & body]
  `(+exists- '~var (fn [~var] ~@body) ~coll))

(defmacro +forall [var coll & body]
  "Universal quantifier syntax.  body is expected to evaluate
  to a heavy-bool"
  `(+forall- '~var (fn [~var] ~@body) ~coll))

(defn +assert [[bool reason :as hb]]
  {:pre [(heavy-bool? hb)]}
  (if (not bool)
    (throw (ex-info (format "%s" reason) {:reason reason
                                          :bool bool}))))

