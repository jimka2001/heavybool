# Table of contents
-  [`gaussian-int`](#gaussian-int) 
    -  [`gaussian-int-mod-p`](#gaussian-int/gaussian-int-mod-p)
    -  [`gaussian?`](#gaussian-int/gaussian?)
-  [`heavy-bool`](#heavy-bool)  - A heavy-bool is a pair [bool reason], where bool is a truth value usually true or false, but may be any clojure truthy or falsey value.
    -  [`+and`](#heavy-bool/+and) - Exapands to code which evaluates to the left-most heavy-bool value in the argument list, otherwise evaluates to the right-most value.
    -  [`+annotate`](#heavy-bool/+annotate) - Eg.
    -  [`+annotate-false`](#heavy-bool/+annotate-false) - Eg.
    -  [`+annotate-true`](#heavy-bool/+annotate-true) - Eg.
    -  [`+assert`](#heavy-bool/+assert)
    -  [`+bool`](#heavy-bool/+bool) - convert heavy-bool to bool.
    -  [`+conj`](#heavy-bool/+conj) - Conjoin an additional item to the reason list.
    -  [`+exists`](#heavy-bool/+exists) - Existential quantifier syntax.
    -  [`+exists-`](#heavy-bool/+exists-) - Function version of +exists.
    -  [`+false`](#heavy-bool/+false)
    -  [`+forall`](#heavy-bool/+forall) - Universal quantifier syntax.
    -  [`+forall-`](#heavy-bool/+forall-) - Functional version of +forall.
    -  [`+heavy-bool`](#heavy-bool/+heavy-bool) - convert bool to heavy-bool.
    -  [`+if`](#heavy-bool/+if) - heavy-bool version of <code>if</code>.
    -  [`+implied-by`](#heavy-bool/+implied-by) - Determine whether a logically implies b.
    -  [`+implies`](#heavy-bool/+implies) - Determine whether a logically implies b.
    -  [`+not`](#heavy-bool/+not) - logically negate the given heavy-bool.
    -  [`+or`](#heavy-bool/+or) - Exapands to code which evaluates to the left-most heavy-bool value in the argument list, otherwise evaluates to the left-most value.
    -  [`+tag`](#heavy-bool/+tag) - Conjoin the given key paired with the boolean value of the given heavy-bool.
    -  [`+true`](#heavy-bool/+true)
    -  [`assert-heavy-bool`](#heavy-bool/assert-heavy-bool)
    -  [`expand-quantifier`](#heavy-bool/expand-quantifier)
    -  [`heavy-bool?`](#heavy-bool/heavy-bool?)
-  [`magma`](#magma) 
    -  [`default-equal`](#magma/default-equal)
    -  [`find-identity`](#magma/find-identity)
    -  [`has-inverses`](#magma/has-inverses)
    -  [`is-associative`](#magma/is-associative)
    -  [`is-closed`](#magma/is-closed)
    -  [`is-commutative`](#magma/is-commutative)
    -  [`is-field`](#magma/is-field)
    -  [`is-group`](#magma/is-group)
    -  [`is-identity`](#magma/is-identity)
    -  [`is-monoid`](#magma/is-monoid)
    -  [`is-ring`](#magma/is-ring)
    -  [`is-semigroup`](#magma/is-semigroup)
-  [`mod-p`](#mod-p) 
    -  [`mod-p`](#mod-p/mod-p) - The non-zero elements of the integers mod p (for prime p) is a group under multiplication.
-  [`relations`](#relations) 
    -  [`is-asymmetric`](#relations/is-asymmetric)
    -  [`is-equivalence`](#relations/is-equivalence)
    -  [`is-irreflexive`](#relations/is-irreflexive)
    -  [`is-reflexive`](#relations/is-reflexive)
    -  [`is-strict-partial-order`](#relations/is-strict-partial-order) - A strict partial order is irreflexive, transitive, and asymmetric.
    -  [`is-symmetric`](#relations/is-symmetric)
    -  [`is-transitive`](#relations/is-transitive) - rel is a binary function which returns a Boolean.
-  [`util`](#util) 
    -  [`*time-out*`](#util/*time-out*)
    -  [`almost-equal`](#util/almost-equal) - Returns a binary function which can be used to test whether two given arguments are within a given tolerance of each other.
    -  [`almost-equal-seq`](#util/almost-equal-seq) - Returns a binary function which can be used to test whether two given sequence arguments are element-wise within a given tolerance of each other.
    -  [`call-with-timeout`](#util/call-with-timeout)
    -  [`find-if`](#util/find-if) - Find the first element in the sequence which makes the predicate true.
    -  [`first-st`](#util/first-st)
    -  [`member`](#util/member) - Determines whether the given target is an element of the given sequence (or given set).
    -  [`re-chunk`](#util/re-chunk) - Given a lazy sequence, change the chunking buffer size to n.
    -  [`tails`](#util/tails) - Return a lazy list of tails of the given collection.
    -  [`testing-with-timeout`](#util/testing-with-timeout)
    -  [`time-call`](#util/time-call) - Evaluates thunk and returns a 2-vector [value elapsed-time] where value is the return value of the thunk and elapsed-time is the number of nano it took evaluating the function.
    -  [`type-check`](#util/type-check)

-----
# <a name="gaussian-int">gaussian-int</a>






## <a name="gaussian-int/gaussian-int-mod-p">`gaussian-int-mod-p`</a><a name="gaussian-int/gaussian-int-mod-p"></a>
``` clojure

(gaussian-int-mod-p p)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/gaussian_int.clj#L11-L122">Source</a></sub></p>

## <a name="gaussian-int/gaussian?">`gaussian?`</a><a name="gaussian-int/gaussian?"></a>
``` clojure

(gaussian? g)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/gaussian_int.clj#L5-L9">Source</a></sub></p>

-----
# <a name="heavy-bool">heavy-bool</a>


A heavy-bool is a pair [bool reason], where bool is a truth value
  usually true or false, but may be any clojure truthy or falsey value.
  reason is a list of maps with keys such as :witness, :bool, and
  :predicate etc.  A heavy-bool answers a predicate question with either
  yes-because or no-because




## <a name="heavy-bool/+and">`+and`</a><a name="heavy-bool/+and"></a>
``` clojure

(+and & rest)
```
Function.

Exapands to code which evaluates to the left-most heavy-bool value
  in the argument list, otherwise evaluates to the right-most
  value.  If the argument list is empty, evaluates explicitly to
  +true
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L49-L63">Source</a></sub></p>

## <a name="heavy-bool/+annotate">`+annotate`</a><a name="heavy-bool/+annotate"></a>
``` clojure

(+annotate heavy-bool & {:as key-vals})
```

Eg. (+annotate hb :x x :y y)
  to add {:x x :y y} as annotation on the given heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L104-L110">Source</a></sub></p>

## <a name="heavy-bool/+annotate-false">`+annotate-false`</a><a name="heavy-bool/+annotate-false"></a>
``` clojure

(+annotate-false heavy-bool & {:as key-vals})
```

Eg. (+annotate-true hb :x x :y y)
  to add {:x x :y y} as annotation on the given heavy-bool if and only if it has false semantics.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L120-L126">Source</a></sub></p>

## <a name="heavy-bool/+annotate-true">`+annotate-true`</a><a name="heavy-bool/+annotate-true"></a>
``` clojure

(+annotate-true heavy-bool & {:as key-vals})
```

Eg. (+annotate-true hb :x x :y y)
  to add {:x x :y y} as annotation on the given heavy-bool if and only if it has true semantics.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L112-L118">Source</a></sub></p>

## <a name="heavy-bool/+assert">`+assert`</a><a name="heavy-bool/+assert"></a>
``` clojure

(+assert [bool reason :as hb])
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L223-L227">Source</a></sub></p>

## <a name="heavy-bool/+bool">`+bool`</a><a name="heavy-bool/+bool"></a>
``` clojure

(+bool [bool reason :as hb])
```

convert heavy-bool to bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L34-L37">Source</a></sub></p>

## <a name="heavy-bool/+conj">`+conj`</a><a name="heavy-bool/+conj"></a>
``` clojure

(+conj hb item)
```

Conjoin an additional item to the reason list
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L95-L102">Source</a></sub></p>

## <a name="heavy-bool/+exists">`+exists`</a><a name="heavy-bool/+exists"></a>
``` clojure

(+exists [v coll & others :as var-coll] & body)
```
Function.

Existential quantifier syntax.  body is expected to evaluate
  to a heavy-bool.  The syntax is similar to `for` and `doseq`
  with :let and :when modifiers being supported but not :while.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L207-L213">Source</a></sub></p>

## <a name="heavy-bool/+exists-">`+exists-`</a><a name="heavy-bool/+exists-"></a>
``` clojure

(+exists- tag f coll)
```

Function version of +exists.
  Traverses the given collection until 1) either an item is found
  which is heavy-true and return it (with a new reason conjoined),
  or 2) else returns explicitly +false.
  If some value in the collection causes the predicate to return
  heavy-true, then a reason will be specified which provides
  the :witness value (the example) which caused the predicate
  to succeed.  The :predicate is also given in the reason.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L160-L173">Source</a></sub></p>

## <a name="heavy-bool/+false">`+false`</a><a name="heavy-bool/+false"></a>



<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L10-L10">Source</a></sub></p>

## <a name="heavy-bool/+forall">`+forall`</a><a name="heavy-bool/+forall"></a>
``` clojure

(+forall [v coll & others :as var-coll] & body)
```
Function.

Universal quantifier syntax.  body is expected to evaluate
  to a heavy-bool.    The syntax is similar to `for` and `doseq`
  with :let and :when modifiers being supported but not :while.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L215-L221">Source</a></sub></p>

## <a name="heavy-bool/+forall-">`+forall-`</a><a name="heavy-bool/+forall-"></a>
``` clojure

(+forall- tag f coll)
```

Functional version of +forall.
  Traverses the given collection until 1) either an item is found
  which is heavy-false and return it (with a new reason conjoined),
  or 2) else +true is returned.
  If some value in the collection causes the predicate to return
  heavy-false, then a reason will be specified which provides
  the :witness value (the counter-example) which caused the predicate
  to fail.  The :predicate is also given in the reason.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L136-L158">Source</a></sub></p>

## <a name="heavy-bool/+heavy-bool">`+heavy-bool`</a><a name="heavy-bool/+heavy-bool"></a>
``` clojure

(+heavy-bool hb)
```

convert bool to heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L27-L32">Source</a></sub></p>

## <a name="heavy-bool/+if">`+if`</a><a name="heavy-bool/+if"></a>
``` clojure

(+if cond consequent alternative)
```
Function.

heavy-bool version of `if`.  The condition must
  evaluate to a heavy-bool.  Either the consequent or
  alternative will be evaluated depending on the heavy-bool
  value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L39-L47">Source</a></sub></p>

## <a name="heavy-bool/+implied-by">`+implied-by`</a><a name="heavy-bool/+implied-by"></a>
``` clojure

(+implied-by b a)
```
Function.

Determine whether a logically implies b.
  a is not evaluated unless b is heavy-false
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L88-L93">Source</a></sub></p>

## <a name="heavy-bool/+implies">`+implies`</a><a name="heavy-bool/+implies"></a>
``` clojure

(+implies a b)
```
Function.

Determine whether a logically implies b.
  b is not evaluated unless a is heavy-true
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L81-L86">Source</a></sub></p>

## <a name="heavy-bool/+not">`+not`</a><a name="heavy-bool/+not"></a>
``` clojure

(+not [bool reason :as hb])
```

logically negate the given heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L20-L25">Source</a></sub></p>

## <a name="heavy-bool/+or">`+or`</a><a name="heavy-bool/+or"></a>
``` clojure

(+or & rest)
```
Function.

Exapands to code which evaluates to the left-most heavy-bool value
  in the argument list, otherwise evaluates to the left-most
  value.  If the argument list is empty, evaluates explicitly to
  +false
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L65-L79">Source</a></sub></p>

## <a name="heavy-bool/+tag">`+tag`</a><a name="heavy-bool/+tag"></a>
``` clojure

(+tag heavy-bool key)
```

Conjoin the given key paired with the boolean value of the given heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L128-L134">Source</a></sub></p>

## <a name="heavy-bool/+true">`+true`</a><a name="heavy-bool/+true"></a>



<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L9-L9">Source</a></sub></p>

## <a name="heavy-bool/assert-heavy-bool">`assert-heavy-bool`</a><a name="heavy-bool/assert-heavy-bool"></a>
``` clojure

(assert-heavy-bool hb)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L176-L178">Source</a></sub></p>

## <a name="heavy-bool/expand-quantifier">`expand-quantifier`</a><a name="heavy-bool/expand-quantifier"></a>
``` clojure

(expand-quantifier var coll others var-coll body macro-name f-name ident)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L180-L205">Source</a></sub></p>

## <a name="heavy-bool/heavy-bool?">`heavy-bool?`</a><a name="heavy-bool/heavy-bool?"></a>
``` clojure

(heavy-bool? heavy-bool)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/heavy_bool.clj#L12-L17">Source</a></sub></p>

-----
# <a name="magma">magma</a>






## <a name="magma/default-equal">`default-equal`</a><a name="magma/default-equal"></a>
``` clojure

(default-equal left right)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L16-L22">Source</a></sub></p>

## <a name="magma/find-identity">`find-identity`</a><a name="magma/find-identity"></a>
``` clojure

(find-identity coll * equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L61-L67">Source</a></sub></p>

## <a name="magma/has-inverses">`has-inverses`</a><a name="magma/has-inverses"></a>
``` clojure

(has-inverses coll * ident invert member equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L91-L111">Source</a></sub></p>

## <a name="magma/is-associative">`is-associative`</a><a name="magma/is-associative"></a>
``` clojure

(is-associative coll * equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L24-L35">Source</a></sub></p>

## <a name="magma/is-closed">`is-closed`</a><a name="magma/is-closed"></a>
``` clojure

(is-closed coll * member)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L5-L14">Source</a></sub></p>

## <a name="magma/is-commutative">`is-commutative`</a><a name="magma/is-commutative"></a>
``` clojure

(is-commutative coll * equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L37-L47">Source</a></sub></p>

## <a name="magma/is-field">`is-field`</a><a name="magma/is-field"></a>
``` clojure

(is-field coll + * zero one +inv *inv member equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L145-L169">Source</a></sub></p>

## <a name="magma/is-group">`is-group`</a><a name="magma/is-group"></a>
``` clojure

(is-group coll * ident invert member equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L113-L122">Source</a></sub></p>

## <a name="magma/is-identity">`is-identity`</a><a name="magma/is-identity"></a>
``` clojure

(is-identity coll * ident equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L49-L59">Source</a></sub></p>

## <a name="magma/is-monoid">`is-monoid`</a><a name="magma/is-monoid"></a>
``` clojure

(is-monoid coll * ident member equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L79-L89">Source</a></sub></p>

## <a name="magma/is-ring">`is-ring`</a><a name="magma/is-ring"></a>
``` clojure

(is-ring coll + * zero one +inv member equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L124-L143">Source</a></sub></p>

## <a name="magma/is-semigroup">`is-semigroup`</a><a name="magma/is-semigroup"></a>
``` clojure

(is-semigroup coll * member equal)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/magma.clj#L69-L77">Source</a></sub></p>

-----
# <a name="mod-p">mod-p</a>






## <a name="mod-p/mod-p">`mod-p`</a><a name="mod-p/mod-p"></a>
``` clojure

(mod-p p)
```

The non-zero elements of the integers mod p (for prime p)
  is a group under multiplication.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/mod_p.clj#L5-L43">Source</a></sub></p>

-----
# <a name="relations">relations</a>






## <a name="relations/is-asymmetric">`is-asymmetric`</a><a name="relations/is-asymmetric"></a>
``` clojure

(is-asymmetric gen rel)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L52-L62">Source</a></sub></p>

## <a name="relations/is-equivalence">`is-equivalence`</a><a name="relations/is-equivalence"></a>
``` clojure

(is-equivalence gen rel)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L42-L49">Source</a></sub></p>

## <a name="relations/is-irreflexive">`is-irreflexive`</a><a name="relations/is-irreflexive"></a>
``` clojure

(is-irreflexive gen rel)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L64-L70">Source</a></sub></p>

## <a name="relations/is-reflexive">`is-reflexive`</a><a name="relations/is-reflexive"></a>
``` clojure

(is-reflexive gen rel)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L5-L12">Source</a></sub></p>

## <a name="relations/is-strict-partial-order">`is-strict-partial-order`</a><a name="relations/is-strict-partial-order"></a>
``` clojure

(is-strict-partial-order gen rel)
```

A strict partial order is irreflexive, transitive, and asymmetric.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L72-L81">Source</a></sub></p>

## <a name="relations/is-symmetric">`is-symmetric`</a><a name="relations/is-symmetric"></a>
``` clojure

(is-symmetric gen rel)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L14-L25">Source</a></sub></p>

## <a name="relations/is-transitive">`is-transitive`</a><a name="relations/is-transitive"></a>
``` clojure

(is-transitive gen rel)
```

rel is a binary function which returns a Boolean
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/relations.clj#L27-L40">Source</a></sub></p>

-----
# <a name="util">util</a>






## <a name="util/*time-out*">`*time-out*`</a><a name="util/*time-out*"></a>



<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L86-L86">Source</a></sub></p>

## <a name="util/almost-equal">`almost-equal`</a><a name="util/almost-equal"></a>
``` clojure

(almost-equal tolerance)
```

Returns a binary function which can be used to test whether two
  given arguments are within a given tolerance of each other.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L37-L46">Source</a></sub></p>

## <a name="util/almost-equal-seq">`almost-equal-seq`</a><a name="util/almost-equal-seq"></a>
``` clojure

(almost-equal-seq tolerance)
```

Returns a binary function which can be used to test whether two
  given sequence arguments are element-wise within a given tolerance of each other.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L48-L56">Source</a></sub></p>

## <a name="util/call-with-timeout">`call-with-timeout`</a><a name="util/call-with-timeout"></a>
``` clojure

(call-with-timeout timeout-ms f)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L88-L100">Source</a></sub></p>

## <a name="util/find-if">`find-if`</a><a name="util/find-if"></a>
``` clojure

(find-if f col)
```

Find the first element in the sequence which makes the predicate true.
  If such an item is found, a singleton list of the item is returned,
  otherwise nil is returned.
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L70-L80">Source</a></sub></p>

## <a name="util/first-st">`first-st`</a><a name="util/first-st"></a>
``` clojure

(first-st var col & body)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L83-L84">Source</a></sub></p>

## <a name="util/member">`member`</a><a name="util/member"></a>
``` clojure

(member target items)
```

Determines whether the given target is an element of the given sequence (or given set).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L23-L34">Source</a></sub></p>

## <a name="util/re-chunk">`re-chunk`</a><a name="util/re-chunk"></a>
``` clojure

(re-chunk n xs)
```

Given a lazy sequence, change the chunking buffer size to n.
  This code was taken directory from
  https://clojuredocs.org/clojure.core/chunk
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L59-L68">Source</a></sub></p>

## <a name="util/tails">`tails`</a><a name="util/tails"></a>
``` clojure

(tails coll)
```

Return a lazy list of tails of the given collection.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L5-L10">Source</a></sub></p>

## <a name="util/testing-with-timeout">`testing-with-timeout`</a><a name="util/testing-with-timeout"></a>
``` clojure

(testing-with-timeout msg & body)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L102-L106">Source</a></sub></p>

## <a name="util/time-call">`time-call`</a><a name="util/time-call"></a>
``` clojure

(time-call thunk)
```

Evaluates thunk and returns a 2-vector [value elapsed-time]
  where value is the return value of the thunk and elapsed-time
  is the number of nano it took evaluating the function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L12-L20">Source</a></sub></p>

## <a name="util/type-check">`type-check`</a><a name="util/type-check"></a>
``` clojure

(type-check f x)
```
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/src/util.clj#L108-L110">Source</a></sub></p>
