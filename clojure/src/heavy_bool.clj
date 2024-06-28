(ns heavy-bool
  "A heavy-bool is a pair [bool reason], where bool is a truth value
  usually true or false, but may be any clojure truthy or falsey value.
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


(defn +not 
  "logically negate the given heavy-bool"
  [[bool reason :as hb]]
  {:pre [(heavy-bool? hb)]
   :post [(heavy-bool? %)]}
  [(not bool) reason])

(defn +heavy-bool
  "convert bool to heavy-bool"
  [hb]
  (if (heavy-bool? hb)
    hb
    [hb ()]))

(defn +bool "convert heavy-bool to bool"
  [[bool reason :as hb]]
  {:pre [(heavy-bool? hb)]}
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
  "Determine whether a logically implies b.
  b is not evaluated unless a is heavy-true"
  [a b]
  `(+or (+not ~a)
       ~b))

(defmacro +implied-by
  "Determine whether a logically implies b.
  a is not evaluated unless b is heavy-false"
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

(defn +annotate 
  "Eg. (+annotate hb :x x :y y)
  to add {:x x :y y} as annotation on the given heavy-bool"
  [heavy-bool & {:as key-vals}]
  {:pre [(heavy-bool? heavy-bool)]
   :post [(heavy-bool? %)]}
  (+conj heavy-bool key-vals))

(defn +annotate-true 
  "Eg. (+annotate-true hb :x x :y y)
  to add {:x x :y y} as annotation on the given heavy-bool if and only if it has true semantics."
  [heavy-bool & {:as key-vals}]
  (+if heavy-bool
       (+conj heavy-bool key-vals)
       heavy-bool))

(defn +annotate-false 
  "Eg. (+annotate-true hb :x x :y y)
  to add {:x x :y y} as annotation on the given heavy-bool if and only if it has false semantics."
  [heavy-bool & {:as key-vals}]
  (+if heavy-bool
       heavy-bool
       (+conj heavy-bool key-vals)))

(defn +tag
  "Conjoin the given key paired with the boolean value of the given heavy-bool"
  [heavy-bool key]
  {:pre [(heavy-bool? heavy-bool)
         (keyword? key)]
   :post [(heavy-bool? %)]}
  (+annotate heavy-bool :key (+bool heavy-bool)))

(defn +forall-
  "Functional version of +forall.
  Traverses the given collection until 1) either an item is found
  which is heavy-false and return it (with a new reason conjoined),
  or 2) else +true is returned.
  If some value in the collection causes the predicate to return
  heavy-false, then a reason will be specified which provides
  the :witness value (the counter-example) which caused the predicate
  to fail.  The :predicate is also given in the reason."
  [tag f coll]
  {:pre [(fn? f)
         (sequential? coll)]
   :post [(heavy-bool? %)]}
  (+annotate 
   (reduce (fn [hb item]
             (assert (heavy-bool? hb))
             (let [this (+heavy-bool (f item))]
               (+if this
                    hb
                    (reduced (+annotate this :witness item)))))
           +true
           coll)
   :tag tag))

(defn +exists- 
  "Function version of +exists.
  Traverses the given collection until 1) either an item is found
  which is heavy-true and return it (with a new reason conjoined),
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
  [[var coll & others] & body]
  (if (empty? others)
    `(+exists- '~var (fn [~var] ~@body) ~coll)
    `(+exists [~var ~coll]
         (+exists [~@others]
             ~@body))))

(defmacro +forall 
  "Universal quantifier syntax.  body is expected to evaluate
  to a heavy-bool"
  [[var coll & others] & body]
  (if (empty? others)
    `(+forall- '~var (fn [~var] ~@body) ~coll)
    `(+forall [~var ~coll]
         (+forall [~@others]
             ~@body))))

(defn +assert [[bool reason :as hb]]
  {:pre [(heavy-bool? hb)]}
  (if (not bool)
    (throw (ex-info (format "%s" reason) {:reason reason
                                          :bool bool}))))

