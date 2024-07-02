# Table of contents
-  [`examples.gaussian-int`](#examples.gaussian-int)  - Example usage of <code>heavy-bool</code>.
    -  [`gaussian-int-mod-p`](#examples.gaussian-int/gaussian-int-mod-p)
    -  [`gaussian?`](#examples.gaussian-int/gaussian?)
-  [`examples.magma`](#examples.magma)  - Example usage of <code>heavy-bool</code>.
    -  [`default-equal`](#examples.magma/default-equal)
    -  [`find-identity`](#examples.magma/find-identity)
    -  [`has-inverses`](#examples.magma/has-inverses)
    -  [`is-associative`](#examples.magma/is-associative)
    -  [`is-closed`](#examples.magma/is-closed)
    -  [`is-commutative`](#examples.magma/is-commutative)
    -  [`is-field`](#examples.magma/is-field)
    -  [`is-group`](#examples.magma/is-group)
    -  [`is-identity`](#examples.magma/is-identity)
    -  [`is-monoid`](#examples.magma/is-monoid)
    -  [`is-ring`](#examples.magma/is-ring)
    -  [`is-semigroup`](#examples.magma/is-semigroup)
-  [`examples.mod-p`](#examples.mod-p)  - Example usage of <code>heavy-bool</code>.
    -  [`mod-p`](#examples.mod-p/mod-p) - The non-zero elements of the integers mod p (for prime p) is a group under multiplication.
-  [`examples.relations`](#examples.relations)  - Example usage of <code>heavy-bool</code>.
    -  [`is-antisymmetric`](#examples.relations/is-antisymmetric)
    -  [`is-asymmetric`](#examples.relations/is-asymmetric) - Test for asymmetric relation.
    -  [`is-equivalence`](#examples.relations/is-equivalence)
    -  [`is-irreflexive`](#examples.relations/is-irreflexive)
    -  [`is-reflexive`](#examples.relations/is-reflexive)
    -  [`is-strict-partial-order`](#examples.relations/is-strict-partial-order) - A strict partial order is irreflexive, transitive, and asymmetric.
    -  [`is-symmetric`](#examples.relations/is-symmetric)
    -  [`is-transitive`](#examples.relations/is-transitive) - rel is a binary function which returns a Boolean.
-  [`heavy-bool`](#heavy-bool)  - A heavy-bool is a pair [bool reason], where bool is a truth value usually true or false, but may be any clojure truthy or falsey value.
    -  [`+and`](#heavy-bool/+and) - Logical AND of heavy-bools which evaluates to a heavy-bool.
    -  [`+annotate`](#heavy-bool/+annotate) - Eg.
    -  [`+annotate-false`](#heavy-bool/+annotate-false) - Eg.
    -  [`+annotate-true`](#heavy-bool/+annotate-true) - Eg.
    -  [`+assert`](#heavy-bool/+assert) - Assert that the given heavy-bool object is logically true.
    -  [`+bool`](#heavy-bool/+bool) - convert heavy-bool to bool.
    -  [`+conj`](#heavy-bool/+conj) - Conjoin an additional item to the reason list.
    -  [`+exists`](#heavy-bool/+exists) - Existential quantifier syntax.
    -  [`+exists-`](#heavy-bool/+exists-) - Function version of <code>+exists</code>.
    -  [`+false`](#heavy-bool/+false) - Standard false heavy-bool value.
    -  [`+forall`](#heavy-bool/+forall) - Universal quantifier syntax.
    -  [`+forall-`](#heavy-bool/+forall-) - Functional version of <code>+forall</code>.
    -  [`+heavy-bool`](#heavy-bool/+heavy-bool) - Constructor (factor function) for heavy-bool.
    -  [`+if`](#heavy-bool/+if) - heavy-bool version of <code>if</code>.
    -  [`+implied-by`](#heavy-bool/+implied-by) - Determine whether heavy-bool <code>a</code> logically implies heavy-bool <code>b</code>.
    -  [`+implies`](#heavy-bool/+implies) - Determine whether heavy-bool <code>a</code> logically implies heavy-bool <code>b</code>.
    -  [`+not`](#heavy-bool/+not) - logically negate the given heavy-bool.
    -  [`+or`](#heavy-bool/+or) - Logical OR of heavy-bools which evaluates to a heavy-bool.
    -  [`+tag`](#heavy-bool/+tag) - Conjoin the given key paired with the boolean value of the given heavy-bool.
    -  [`+true`](#heavy-bool/+true) - Standard true heavy-bool value.
    -  [`assert-heavy-bool`](#heavy-bool/assert-heavy-bool) - Assert that a given object is a heavy-bool.
    -  [`expand-quantifier`](#heavy-bool/expand-quantifier) - Helper function used in the macro expansion of <code>+exists</code> and <code>+forall</code>.
    -  [`heavy-bool?`](#heavy-bool/heavy-bool?) - Predicate returning true if the given object is a <code>heavy-bool</code>.
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
# <a name="examples.gaussian-int">examples.gaussian-int</a>


Example usage of [`heavy-bool`](#heavy-bool).
  This namespace implements Gaussian Integers [Gaussian integer](https://en.wikipedia.org/wiki/Gaussian_integer)
  but restricted to integers modulo some prime, p.




## <a name="examples.gaussian-int/gaussian-int-mod-p">`gaussian-int-mod-p`</a><a name="examples.gaussian-int/gaussian-int-mod-p"></a>
``` clojure

(gaussian-int-mod-p p)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/gaussian_int.clj#L14-L125">Source</a></sub></p>

## <a name="examples.gaussian-int/gaussian?">`gaussian?`</a><a name="examples.gaussian-int/gaussian?"></a>
``` clojure

(gaussian? g)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/gaussian_int.clj#L8-L12">Source</a></sub></p>

-----
# <a name="examples.magma">examples.magma</a>


Example usage of [`heavy-bool`](#heavy-bool).
  This namespace implements tests for certain finite algebraic structures including:
  magma, semigroup, monoid, group, ring, and field.
  In each case we assume that the elements of the algebraic structures form a finite
  set.  Thus we may test the axioms, such as closure, associativity, and identity,
  using exhaustive seach.




## <a name="examples.magma/default-equal">`default-equal`</a><a name="examples.magma/default-equal"></a>
``` clojure

(default-equal left right)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L22-L28">Source</a></sub></p>

## <a name="examples.magma/find-identity">`find-identity`</a><a name="examples.magma/find-identity"></a>
``` clojure

(find-identity coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L67-L73">Source</a></sub></p>

## <a name="examples.magma/has-inverses">`has-inverses`</a><a name="examples.magma/has-inverses"></a>
``` clojure

(has-inverses coll * ident invert member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L97-L117">Source</a></sub></p>

## <a name="examples.magma/is-associative">`is-associative`</a><a name="examples.magma/is-associative"></a>
``` clojure

(is-associative coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L30-L41">Source</a></sub></p>

## <a name="examples.magma/is-closed">`is-closed`</a><a name="examples.magma/is-closed"></a>
``` clojure

(is-closed coll * member)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L11-L20">Source</a></sub></p>

## <a name="examples.magma/is-commutative">`is-commutative`</a><a name="examples.magma/is-commutative"></a>
``` clojure

(is-commutative coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L43-L53">Source</a></sub></p>

## <a name="examples.magma/is-field">`is-field`</a><a name="examples.magma/is-field"></a>
``` clojure

(is-field coll + * zero one +inv *inv member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L151-L175">Source</a></sub></p>

## <a name="examples.magma/is-group">`is-group`</a><a name="examples.magma/is-group"></a>
``` clojure

(is-group coll * ident invert member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L119-L128">Source</a></sub></p>

## <a name="examples.magma/is-identity">`is-identity`</a><a name="examples.magma/is-identity"></a>
``` clojure

(is-identity coll * ident equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L55-L65">Source</a></sub></p>

## <a name="examples.magma/is-monoid">`is-monoid`</a><a name="examples.magma/is-monoid"></a>
``` clojure

(is-monoid coll * ident member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L85-L95">Source</a></sub></p>

## <a name="examples.magma/is-ring">`is-ring`</a><a name="examples.magma/is-ring"></a>
``` clojure

(is-ring coll + * zero one +inv member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L130-L149">Source</a></sub></p>

## <a name="examples.magma/is-semigroup">`is-semigroup`</a><a name="examples.magma/is-semigroup"></a>
``` clojure

(is-semigroup coll * member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/magma.clj#L75-L83">Source</a></sub></p>

-----
# <a name="examples.mod-p">examples.mod-p</a>


Example usage of [`heavy-bool`](#heavy-bool).
  This namespace implements an algebraic structure [`mod-p`](#examples.mod-p/mod-p).
  This structure is useful for testing the [`examples.magma/is-group`](#examples.magma/is-group) function.
  




## <a name="examples.mod-p/mod-p">`mod-p`</a><a name="examples.mod-p/mod-p"></a>
``` clojure

(mod-p p)
```
Function.

The non-zero elements of the integers mod p (for prime p)
  is a group under multiplication.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/mod_p.clj#L9-L47">Source</a></sub></p>

-----
# <a name="examples.relations">examples.relations</a>


Example usage of [`heavy-bool`](#heavy-bool).
  This namespace defines several relations such as reflexive, symmetric, and antisymmetric.
  




## <a name="examples.relations/is-antisymmetric">`is-antisymmetric`</a><a name="examples.relations/is-antisymmetric"></a>
``` clojure

(is-antisymmetric gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L69-L80">Source</a></sub></p>

## <a name="examples.relations/is-asymmetric">`is-asymmetric`</a><a name="examples.relations/is-asymmetric"></a>
``` clojure

(is-asymmetric gen rel)
```
Function.

Test for asymmetric relation
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L55-L67">Source</a></sub></p>

## <a name="examples.relations/is-equivalence">`is-equivalence`</a><a name="examples.relations/is-equivalence"></a>
``` clojure

(is-equivalence gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L45-L52">Source</a></sub></p>

## <a name="examples.relations/is-irreflexive">`is-irreflexive`</a><a name="examples.relations/is-irreflexive"></a>
``` clojure

(is-irreflexive gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L82-L88">Source</a></sub></p>

## <a name="examples.relations/is-reflexive">`is-reflexive`</a><a name="examples.relations/is-reflexive"></a>
``` clojure

(is-reflexive gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L8-L15">Source</a></sub></p>

## <a name="examples.relations/is-strict-partial-order">`is-strict-partial-order`</a><a name="examples.relations/is-strict-partial-order"></a>
``` clojure

(is-strict-partial-order gen rel)
```
Function.

A strict partial order is irreflexive, transitive, and asymmetric.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L90-L99">Source</a></sub></p>

## <a name="examples.relations/is-symmetric">`is-symmetric`</a><a name="examples.relations/is-symmetric"></a>
``` clojure

(is-symmetric gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L17-L28">Source</a></sub></p>

## <a name="examples.relations/is-transitive">`is-transitive`</a><a name="examples.relations/is-transitive"></a>
``` clojure

(is-transitive gen rel)
```
Function.

rel is a binary function which returns a Boolean
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/examples/relations.clj#L30-L43">Source</a></sub></p>

-----
# <a name="heavy-bool">heavy-bool</a>


A heavy-bool is a pair [bool reason], where bool is a truth value
  usually true or false, but may be any clojure truthy or falsey value.
  reason is a list of maps with keys such as `:witness`, `:bool`, and
  `:predicate` etc.  A heavy-bool answers a predicate question with either
  yes-because or no-because




## <a name="heavy-bool/+and">`+and`</a><a name="heavy-bool/+and"></a>
``` clojure

(+and & rest)
```
Macro.

Logical AND of heavy-bools which evaluates to a heavy-bool.
  Expands to code which evaluates to the left-most heavy-bool value
  in the argument list, otherwise evaluates to the right-most
  value.  If the argument list is empty, evaluates explicitly to
  +true
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L52-L67">Source</a></sub></p>

## <a name="heavy-bool/+annotate">`+annotate`</a><a name="heavy-bool/+annotate"></a>
``` clojure

(+annotate heavy-bool & {:as key-vals})
```
Function.

Eg. `(+annotate hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L109-L115">Source</a></sub></p>

## <a name="heavy-bool/+annotate-false">`+annotate-false`</a><a name="heavy-bool/+annotate-false"></a>
``` clojure

(+annotate-false heavy-bool & {:as key-vals})
```
Function.

Eg. `(+annotate-true hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given heavy-bool if and only if it has false semantics.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L125-L131">Source</a></sub></p>

## <a name="heavy-bool/+annotate-true">`+annotate-true`</a><a name="heavy-bool/+annotate-true"></a>
``` clojure

(+annotate-true heavy-bool & {:as key-vals})
```
Function.

Eg. `(+annotate-true hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given heavy-bool if and only if it has true semantics.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L117-L123">Source</a></sub></p>

## <a name="heavy-bool/+assert">`+assert`</a><a name="heavy-bool/+assert"></a>
``` clojure

(+assert [bool reason :as hb])
```
Function.

Assert that the given heavy-bool object is logically true
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L232-L238">Source</a></sub></p>

## <a name="heavy-bool/+bool">`+bool`</a><a name="heavy-bool/+bool"></a>
``` clojure

(+bool [bool reason :as hb])
```
Function.

convert heavy-bool to bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L37-L40">Source</a></sub></p>

## <a name="heavy-bool/+conj">`+conj`</a><a name="heavy-bool/+conj"></a>
``` clojure

(+conj hb item)
```
Function.

Conjoin an additional item to the reason list
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L100-L107">Source</a></sub></p>

## <a name="heavy-bool/+exists">`+exists`</a><a name="heavy-bool/+exists"></a>
``` clojure

(+exists [v coll & others :as var-coll] & body)
```
Macro.

Existential quantifier syntax.  `body` is expected to evaluate
  to a heavy-bool.  The syntax is similar to `for` and `doseq`
  with `:let` and `:when` modifiers being supported but not `:while`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L216-L222">Source</a></sub></p>

## <a name="heavy-bool/+exists-">`+exists-`</a><a name="heavy-bool/+exists-"></a>
``` clojure

(+exists- tag f coll)
```
Function.

Function version of [`+exists`](#heavy-bool/+exists).
  Traverses the given collection until 1) either an item is found
  which is heavy-true and return it (with a new reason conjoined),
  or 2) else returns explicitly [`+false`](#heavy-bool/+false).
  If some value in the collection causes the predicate to return
  heavy-true, then a reason will be specified which provides
  the `:witness` value (the example) which caused the predicate
  to succeed.  The `:predicate` is also given in the reason.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L165-L178">Source</a></sub></p>

## <a name="heavy-bool/+false">`+false`</a><a name="heavy-bool/+false"></a>




Standard false heavy-bool value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L10-L10">Source</a></sub></p>

## <a name="heavy-bool/+forall">`+forall`</a><a name="heavy-bool/+forall"></a>
``` clojure

(+forall [v coll & others :as var-coll] & body)
```
Macro.

Universal quantifier syntax.  `body` is expected to evaluate
  to a heavy-bool.    The syntax is similar to `for` and `doseq`
  with `:let` and `:when` modifiers being supported but not `:while`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L224-L230">Source</a></sub></p>

## <a name="heavy-bool/+forall-">`+forall-`</a><a name="heavy-bool/+forall-"></a>
``` clojure

(+forall- tag f coll)
```
Function.

Functional version of [`+forall`](#heavy-bool/+forall).
  Traverses the given collection until 1) either an item is found
  which is heavy-false and return it (with a new reason conjoined),
  or 2) else [`+true`](#heavy-bool/+true) is returned.
  If some value in the collection causes the predicate to return
  heavy-false, then a reason will be specified which provides
  the `:witness` value (the counter-example) which caused the predicate
  to fail.  The `:predicate` is also given in the reason.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L141-L163">Source</a></sub></p>

## <a name="heavy-bool/+heavy-bool">`+heavy-bool`</a><a name="heavy-bool/+heavy-bool"></a>
``` clojure

(+heavy-bool hb)
```
Function.

Constructor (factor function) for heavy-bool.
  convert bool to heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L29-L35">Source</a></sub></p>

## <a name="heavy-bool/+if">`+if`</a><a name="heavy-bool/+if"></a>
``` clojure

(+if cond consequent alternative)
```
Macro.

heavy-bool version of `if`.  The condition must
  evaluate to a heavy-bool.  Either the consequent or
  alternative will be evaluated depending on the heavy-bool
  value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L42-L50">Source</a></sub></p>

## <a name="heavy-bool/+implied-by">`+implied-by`</a><a name="heavy-bool/+implied-by"></a>
``` clojure

(+implied-by b a)
```
Macro.

Determine whether heavy-bool `a` logically implies heavy-bool `b`.
  `a` is not evaluated unless `b` is heavy-false
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L93-L98">Source</a></sub></p>

## <a name="heavy-bool/+implies">`+implies`</a><a name="heavy-bool/+implies"></a>
``` clojure

(+implies a b)
```
Macro.

Determine whether heavy-bool `a` logically implies heavy-bool `b`.
  `b` is not evaluated unless `a` is heavy-true
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L86-L91">Source</a></sub></p>

## <a name="heavy-bool/+not">`+not`</a><a name="heavy-bool/+not"></a>
``` clojure

(+not [bool reason :as hb])
```
Function.

logically negate the given heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L22-L27">Source</a></sub></p>

## <a name="heavy-bool/+or">`+or`</a><a name="heavy-bool/+or"></a>
``` clojure

(+or & rest)
```
Macro.

Logical OR of heavy-bools which evaluates to a heavy-bool.
  Expands to code which evaluates to the left-most heavy-bool value
  in the argument list, otherwise evaluates to the left-most
  value.  If the argument list is empty, evaluates explicitly to
  +false
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L69-L84">Source</a></sub></p>

## <a name="heavy-bool/+tag">`+tag`</a><a name="heavy-bool/+tag"></a>
``` clojure

(+tag heavy-bool key)
```
Function.

Conjoin the given key paired with the boolean value of the given heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L133-L139">Source</a></sub></p>

## <a name="heavy-bool/+true">`+true`</a><a name="heavy-bool/+true"></a>




Standard true heavy-bool value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L9-L9">Source</a></sub></p>

## <a name="heavy-bool/assert-heavy-bool">`assert-heavy-bool`</a><a name="heavy-bool/assert-heavy-bool"></a>
``` clojure

(assert-heavy-bool hb)
```
Function.

Assert that a given object is a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L181-L185">Source</a></sub></p>

## <a name="heavy-bool/expand-quantifier">`expand-quantifier`</a><a name="heavy-bool/expand-quantifier"></a>
``` clojure

(expand-quantifier var coll others var-coll body macro-name f-name ident)
```
Function.

Helper function used in the macro expansion of [`+exists`](#heavy-bool/+exists) and [`+forall`](#heavy-bool/+forall)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L187-L214">Source</a></sub></p>

## <a name="heavy-bool/heavy-bool?">`heavy-bool?`</a><a name="heavy-bool/heavy-bool?"></a>
``` clojure

(heavy-bool? heavy-bool)
```
Function.

Predicate returning true if the given object is a [`heavy-bool`](#heavy-bool)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L12-L19">Source</a></sub></p>

-----
# <a name="util">util</a>






## <a name="util/*time-out*">`*time-out*`</a><a name="util/*time-out*"></a>



<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L86-L86">Source</a></sub></p>

## <a name="util/almost-equal">`almost-equal`</a><a name="util/almost-equal"></a>
``` clojure

(almost-equal tolerance)
```
Function.

Returns a binary function which can be used to test whether two
  given arguments are within a given tolerance of each other.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L37-L46">Source</a></sub></p>

## <a name="util/almost-equal-seq">`almost-equal-seq`</a><a name="util/almost-equal-seq"></a>
``` clojure

(almost-equal-seq tolerance)
```
Function.

Returns a binary function which can be used to test whether two
  given sequence arguments are element-wise within a given tolerance of each other.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L48-L56">Source</a></sub></p>

## <a name="util/call-with-timeout">`call-with-timeout`</a><a name="util/call-with-timeout"></a>
``` clojure

(call-with-timeout timeout-ms f)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L88-L100">Source</a></sub></p>

## <a name="util/find-if">`find-if`</a><a name="util/find-if"></a>
``` clojure

(find-if f col)
```
Function.

Find the first element in the sequence which makes the predicate true.
  If such an item is found, a singleton list of the item is returned,
  otherwise nil is returned.
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L70-L80">Source</a></sub></p>

## <a name="util/first-st">`first-st`</a><a name="util/first-st"></a>
``` clojure

(first-st var col & body)
```
Macro.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L83-L84">Source</a></sub></p>

## <a name="util/member">`member`</a><a name="util/member"></a>
``` clojure

(member target items)
```
Function.

Determines whether the given target is an element of the given sequence (or given set).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L23-L34">Source</a></sub></p>

## <a name="util/re-chunk">`re-chunk`</a><a name="util/re-chunk"></a>
``` clojure

(re-chunk n xs)
```
Function.

Given a lazy sequence, change the chunking buffer size to n.
  This code was taken directory from
  https://clojuredocs.org/clojure.core/chunk
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L59-L68">Source</a></sub></p>

## <a name="util/tails">`tails`</a><a name="util/tails"></a>
``` clojure

(tails coll)
```
Function.

Return a lazy list of tails of the given collection.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L5-L10">Source</a></sub></p>

## <a name="util/testing-with-timeout">`testing-with-timeout`</a><a name="util/testing-with-timeout"></a>
``` clojure

(testing-with-timeout msg & body)
```
Macro.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L102-L106">Source</a></sub></p>

## <a name="util/time-call">`time-call`</a><a name="util/time-call"></a>
``` clojure

(time-call thunk)
```
Function.

Evaluates thunk and returns a 2-vector [value elapsed-time]
  where value is the return value of the thunk and elapsed-time
  is the number of nano it took evaluating the function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L12-L20">Source</a></sub></p>

## <a name="util/type-check">`type-check`</a><a name="util/type-check"></a>
``` clojure

(type-check f x)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L108-L110">Source</a></sub></p>
