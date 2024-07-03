(ns heavy-bool
  "A `heavy-bool` is a pair `[bool reason]`, where `bool` is a truth value
  usually true or false, but may be any clojure truthy or falsey value.
  `reason` is a list of maps with keys such as `:witness`, `:bool`, and
  `:predicate` etc.  A `heavy-bool` answers a predicate question with either
  yes-because or no-because")


(def +true "Standard true heavy-bool value." [true ()])
(def +false "Standard false heavy-bool value." [false ()])

(defn heavy-bool? 
  "Predicate returning true if the given object is a `heavy-bool`, false otherwise." 
  [heavy-bool]
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

(defn +conj
  "Conjoin an additional item to the reason list"
  [hb item]
  {:pre [(heavy-bool? hb)
         (map? item)]
   :post [(heavy-bool? %)]}
  (let [[bool reason] hb]
    [bool (conj reason item)]))

(defn +heavy-bool
  "Constructor (factor function) for `heavy-bool`.
  convert `arg` to a `heavy-bool`.
  `true` is converted to `+true`
  `false` is converted to `+false`
  If `arg` is already a `heavy-bool`, no conversion occurs.
  In any case, if other key/value pairs are given, they are conjoined via `+conj`
  "
  ([arg]
   {:pre [(or (boolean? arg) (heavy-bool? arg))]
    :post [(heavy-bool? %)]}
   (if (boolean? arg)
     [arg ()]
     arg))
  ([arg & {:as key-vals}]
   {:pre [(or (boolean? arg) (heavy-bool? arg))]
    :post [(heavy-bool? %)]}
   (if (boolean? arg)
     (+conj (+heavy-bool arg) key-vals)
     (+conj arg key-vals))))

(defn find-reason
  "Search the reasons (each a map) within a heavy-bool for the first
  one that has the given key.  When found, return the value of the key."
  [hb key]
  {:pre [(keyword? key)
         (heavy-bool? hb)]}
  (loop [[reason-1 & other-reasons :as reasons] (second hb)]
    (cond (empty? reasons)
          nil

          (contains? reason-1 key)
          (key reason-1)

          :else
          (recur other-reasons))))


(defn +bool
  "convert a `heavy-bool` to explictly `true` or `false`."
  [[bool reason :as hb]]
  {:pre [(heavy-bool? hb)]
   :post [(boolean? %)]
   }
  (boolean bool))


(defmacro +if
  "heavy-bool version of `if`.  The condition must
  evaluate to a `heavy-bool`.  Either the consequent or
  alternative will be evaluated depending on the `heavy-bool`
  value."
  [cond consequent alternative]
  `(if (+bool ~cond)
     ~consequent
     ~alternative))

(defmacro +and
  "Logical AND of heavy-bools which evaluates to a `heavy-bool`.
  Expands to code which evaluates to the left-most `heavy-bool` value
  in the argument list, otherwise evaluates to the right-most
  value.  If the argument list is empty, evaluates explicitly to
  `+true`"
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
  "Logical OR of heavy-bools which evaluates to a `heavy-bool`.
  Expands to code which evaluates to the left-most `heavy-bool` value
  in the argument list, otherwise evaluates to the left-most
  value.  If the argument list is empty, evaluates explicitly to
  `+false`"
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
  "Determine whether heavy-bool `a` logically implies heavy-bool `b`.
  `b` is not evaluated unless `a` is heavy-true"
  [a b]
  `(+or (+not ~a)
       ~b))

(defmacro +implied-by
  "Determine whether heavy-bool `a` logically implies heavy-bool `b`.
  `a` is not evaluated unless `b` is heavy-false"
  [b a]
  `(+or ~b
        (+not ~a)))

(defn +annotate 
  "Add key/value pairs as annotation to a heavy-bool.
  Eg. `(+annotate hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given heavy-bool"
  [heavy-bool & {:as key-vals}]
  {:pre [(heavy-bool? heavy-bool)]
   :post [(heavy-bool? %)]}
  (+conj heavy-bool key-vals))

(defn +annotate-true 
  "Add key/value pairs as annotation to a true heavy-bool.
  Eg. `(+annotate-true hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given heavy-bool if and only if it has true semantics."
  [heavy-bool & {:as key-vals}]
  (+if heavy-bool
       (+conj heavy-bool key-vals)
       heavy-bool))

(defn +annotate-false 
  "Add key/value pairs as annotation to a false heavy-bool.
  Eg. `(+annotate-true hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given heavy-bool if and only if it has false semantics."
  [heavy-bool & {:as key-vals}]
  (+if heavy-bool
       heavy-bool
       (+conj heavy-bool key-vals)))

(defn +tag
  "Conjoin the given key paired with the Boolean value of the given heavy-bool"
  [heavy-bool key]
  {:pre [(heavy-bool? heavy-bool)
         (keyword? key)]
   :post [(heavy-bool? %)]}
  (+annotate heavy-bool key (+bool heavy-bool)))

(defn +forall-impl
  "Functional version of `+forall`.
  Traverses the given collection until 1) either an item is found
  which is heavy-false and return it (with a new reason conjoined),
  or 2) else `+true` is returned.
  If some value in the collection causes the predicate to return
  heavy-false, then a reason will be specified which provides
  the `:witness` value (the counter-example) which caused the predicate
  to fail.  The `:predicate` is also given in the reason."
  [tag f coll]
  {:pre [(fn? f)
         (sequential? coll)]
   :post [(heavy-bool? %)]}
  (+annotate-true
   (reduce (fn [hb item]
             (assert (heavy-bool? hb))
             (let [this (+heavy-bool (f item))]
               (+if this
                    hb
                    (reduced (+annotate this :witness item :var tag)))))
           +true
           coll)
   :var tag))

(defn +exists-impl
  "Function version of `+exists`.
  Traverses the given collection until 1) either an item is found
  which is heavy-true and return it (with a new reason conjoined),
  or 2) else returns explicitly `+false`.
  If some value in the collection causes the predicate to return
  heavy-true, then a reason will be specified which provides
  the `:witness` value (the example) which caused the predicate
  to succeed.  The `:predicate` is also given in the reason.
  `f` is a function which either returns a boolean (explicitly `true` or `false`)
       or returns a `heavy-bool`
  "
  [tag f coll]
  {:pre [(fn? f)
         (sequential? coll)]
   :post [(heavy-bool? %)]}
  (+not (+forall-impl tag (fn [x] (+not (+heavy-bool (f x)))) coll)))


(defn assert-heavy-bool
  "Assert that a given object is a `heavy-bool`."
  [hb]
  (assert (heavy-bool? hb) (format "expecting heavy-bool, got %s" hb))
  true)

(defn- expand-quantifier
  "Helper function used in the macro expansion of `+exists` and `+forall`"
  [var coll others var-coll body
   macro-name f-name ident]
  (cond (empty? var-coll)
        `(do ~@body)

        (= var :let)
        `(let ~coll
           (~macro-name [~@others]
               ~@body))

        (= var :when)
        `(if ~coll
           (~macro-name [~@others]
               ~@body)
           ~ident)
        
        (empty? others)
        `(~f-name '~var
          (fn [~var] ~@body)
          ~coll)

        :else
        `(~macro-name [~var ~coll]
             (~macro-name [~@others]
                 ~@body))))

(defmacro +exists
  "Existential quantifier syntax.  `body` is expected to evaluate
  to a heavy-bool.  The syntax is similar to `for` and `doseq`
  with `:let` and `:when` modifiers being supported but not `:while`."
  [[v coll & others :as var-coll] & body]
  (expand-quantifier v coll others var-coll body
                     `+exists `+exists-impl `+false))

(defmacro +forall 
  "Universal quantifier syntax.  `body` is expected to evaluate
  to a heavy-bool.    The syntax is similar to `for` and `doseq`
  with `:let` and `:when` modifiers being supported but not `:while`."
  [[v coll & others :as var-coll] & body]
  (expand-quantifier v coll others var-coll body
                     `+forall `+forall-impl `+true))

(defn +assert 
  "Assert that the given heavy-bool object is logically true"
  [[bool reason :as hb]]
  {:pre [(heavy-bool? hb)]}
  (if (not bool)
    (throw (ex-info (format "%s" reason) {:reason reason
                                          :bool bool}))))

