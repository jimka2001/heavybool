# Table of contents
-  [`gaussian-int`](#gaussian-int)  - Example usage of <code>heavy-bool</code>.
    -  [`gaussian-int-mod-p`](#gaussian-int/gaussian-int-mod-p)
    -  [`gaussian?`](#gaussian-int/gaussian?)
-  [`magma`](#magma)  - Example usage of <code>heavy-bool</code>.
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
-  [`mod-p`](#mod-p)  - Example usage of <code>heavy-bool</code>.
    -  [`mod-p`](#mod-p/mod-p) - The non-zero elements of the integers mod p (for prime p) is a group under multiplication.
-  [`relations`](#relations)  - Example usage of <code>heavy-bool</code>.
    -  [`is-antisymmetric`](#relations/is-antisymmetric) - Test for antisymmetric relation: ((a R b) and (b R a)) => (a = b) <code>gen</code> is a collection <code>hb-rel</code> is a binary function returning a heavy-bool.
    -  [`is-asymmetric`](#relations/is-asymmetric) - Test for asymmetric relation.
    -  [`is-connected`](#relations/is-connected) - A connected relation means that if x!=y then (x R y) or (y R x) <code>gen</code> is a collection <code>hb-rel</code> is a binary function returning a heavy-bool.
    -  [`is-equivalence`](#relations/is-equivalence) - An equivalence relation is symmetric, reflexive, and transitive.
    -  [`is-irreflexive`](#relations/is-irreflexive) - An irreflexive relation is a relation for which (x R x) is always false <code>gen</code> is a collection <code>hb-rel</code> is a binary function returning a heavy-bool.
    -  [`is-partial-order`](#relations/is-partial-order) - A partial order (or weak partial order) relation is reflexive, antisymmetric, and transitive.
    -  [`is-reflexive`](#relations/is-reflexive) - A reflexive relation is a relation for (a R a) always <code>gen</code> is a collection <code>hb-rel</code> is a binary function returning a heavy-bool.
    -  [`is-strict-partial-order`](#relations/is-strict-partial-order) - A strict partial order is irreflexive, transitive, and asymmetric.
    -  [`is-strongly-connected`](#relations/is-strongly-connected) - A strongly connected relation means that (x R y) or (y R x) <code>gen</code> is a collection <code>hb-rel</code> is a binary function returning a heavy-bool.
    -  [`is-symmetric`](#relations/is-symmetric) - A symmetric relation means that (a R b) => (b R a).
    -  [`is-transitive`](#relations/is-transitive) - hb-rel is a binary function which returns a <code>heavy-bool</code> <code>gen</code> is a collection <code>hb-rel</code> is a binary function returning a heavy-bool.
    -  [`lift-relation`](#relations/lift-relation) - Accepts a relation, a binary predicate returning a boolean, and returns a binary predicate returning a heavy-boolean.

-----
# <a name="gaussian-int">gaussian-int</a>


Example usage of `heavy-bool`.
  This namespace implements Gaussian Integers [Gaussian integer](https://en.wikipedia.org/wiki/Gaussian_integer)
  but restricted to integers modulo some prime, p.




## <a name="gaussian-int/gaussian-int-mod-p">`gaussian-int-mod-p`</a><a name="gaussian-int/gaussian-int-mod-p"></a>
``` clojure

(gaussian-int-mod-p p)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/gaussian_int.clj#L14-L125">Source</a></sub></p>

## <a name="gaussian-int/gaussian?">`gaussian?`</a><a name="gaussian-int/gaussian?"></a>
``` clojure

(gaussian? g)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/gaussian_int.clj#L8-L12">Source</a></sub></p>

-----
# <a name="magma">magma</a>


Example usage of `heavy-bool`.
  This namespace implements tests for certain finite algebraic structures including:
  magma, semigroup, monoid, group, ring, and field.
  In each case we assume that the elements of the algebraic structures form a finite
  set.  Thus we may test the axioms, such as closure, associativity, and identity,
  using exhaustive seach.




## <a name="magma/default-equal">`default-equal`</a><a name="magma/default-equal"></a>
``` clojure

(default-equal left right)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L23-L29">Source</a></sub></p>

## <a name="magma/find-identity">`find-identity`</a><a name="magma/find-identity"></a>
``` clojure

(find-identity coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L68-L74">Source</a></sub></p>

## <a name="magma/has-inverses">`has-inverses`</a><a name="magma/has-inverses"></a>
``` clojure

(has-inverses coll * ident invert member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L98-L116">Source</a></sub></p>

## <a name="magma/is-associative">`is-associative`</a><a name="magma/is-associative"></a>
``` clojure

(is-associative coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L31-L42">Source</a></sub></p>

## <a name="magma/is-closed">`is-closed`</a><a name="magma/is-closed"></a>
``` clojure

(is-closed coll * member)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L12-L21">Source</a></sub></p>

## <a name="magma/is-commutative">`is-commutative`</a><a name="magma/is-commutative"></a>
``` clojure

(is-commutative coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L44-L54">Source</a></sub></p>

## <a name="magma/is-field">`is-field`</a><a name="magma/is-field"></a>
``` clojure

(is-field coll + * zero one +inv *inv member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L150-L174">Source</a></sub></p>

## <a name="magma/is-group">`is-group`</a><a name="magma/is-group"></a>
``` clojure

(is-group coll * ident invert member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L118-L127">Source</a></sub></p>

## <a name="magma/is-identity">`is-identity`</a><a name="magma/is-identity"></a>
``` clojure

(is-identity coll * ident equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L56-L66">Source</a></sub></p>

## <a name="magma/is-monoid">`is-monoid`</a><a name="magma/is-monoid"></a>
``` clojure

(is-monoid coll * ident member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L86-L96">Source</a></sub></p>

## <a name="magma/is-ring">`is-ring`</a><a name="magma/is-ring"></a>
``` clojure

(is-ring coll + * zero one +inv member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L129-L148">Source</a></sub></p>

## <a name="magma/is-semigroup">`is-semigroup`</a><a name="magma/is-semigroup"></a>
``` clojure

(is-semigroup coll * member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L76-L84">Source</a></sub></p>

-----
# <a name="mod-p">mod-p</a>


Example usage of `heavy-bool`.
  This namespace implements an algebraic structure [`mod-p`](#mod-p).
  This structure is useful for testing the `examples.magma/is-group` function.
  




## <a name="mod-p/mod-p">`mod-p`</a><a name="mod-p/mod-p"></a>
``` clojure

(mod-p p)
```
Function.

The non-zero elements of the integers mod p (for prime p)
  is a group under multiplication.
  This function returns a map with the keys:
    :p  -- positive integer for which the operation is performed modulo p
    :gen -- a collection of integers from 1 to p-1
    :equiv -- a heavy-boolean relation (binary function returning a `heavy-bool`)
    :invert -- 
    :member
    :op
    :ident
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/mod_p.clj#L10-L56">Source</a></sub></p>

-----
# <a name="relations">relations</a>


Example usage of `heavy-bool`.
  This namespace defines several relations such as reflexive, symmetric, and antisymmetric.
  




## <a name="relations/is-antisymmetric">`is-antisymmetric`</a><a name="relations/is-antisymmetric"></a>
``` clojure

(is-antisymmetric gen hb-rel)
```
Function.

Test for antisymmetric relation:
  ((a R b) and (b R a)) => (a = b)
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L80-L95">Source</a></sub></p>

## <a name="relations/is-asymmetric">`is-asymmetric`</a><a name="relations/is-asymmetric"></a>
``` clojure

(is-asymmetric gen hb-rel)
```
Function.

Test for asymmetric relation. (a R b) => (not (b R a))
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L66-L78">Source</a></sub></p>

## <a name="relations/is-connected">`is-connected`</a><a name="relations/is-connected"></a>
``` clojure

(is-connected gen hb-rel)
```
Function.

A connected relation means that if x!=y then (x R y) or (y R x)
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L136-L149">Source</a></sub></p>

## <a name="relations/is-equivalence">`is-equivalence`</a><a name="relations/is-equivalence"></a>
``` clojure

(is-equivalence gen hb-rel)
```
Function.

An equivalence relation is symmetric, reflexive, and transitive.
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L52-L63">Source</a></sub></p>

## <a name="relations/is-irreflexive">`is-irreflexive`</a><a name="relations/is-irreflexive"></a>
``` clojure

(is-irreflexive gen hb-rel)
```
Function.

An irreflexive relation is a relation for which (x R x) is always false
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L97-L107">Source</a></sub></p>

## <a name="relations/is-partial-order">`is-partial-order`</a><a name="relations/is-partial-order"></a>
``` clojure

(is-partial-order gen hb-rel)
```
Function.

A partial order (or weak partial order) relation is
  reflexive, antisymmetric, and transitive.
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L109-L121">Source</a></sub></p>

## <a name="relations/is-reflexive">`is-reflexive`</a><a name="relations/is-reflexive"></a>
``` clojure

(is-reflexive gen hb-rel)
```
Function.

A reflexive relation is a relation for (a R a) always
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L8-L18">Source</a></sub></p>

## <a name="relations/is-strict-partial-order">`is-strict-partial-order`</a><a name="relations/is-strict-partial-order"></a>
``` clojure

(is-strict-partial-order gen hb-rel)
```
Function.

A strict partial order is irreflexive, transitive, and asymmetric.
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L123-L134">Source</a></sub></p>

## <a name="relations/is-strongly-connected">`is-strongly-connected`</a><a name="relations/is-strongly-connected"></a>
``` clojure

(is-strongly-connected gen hb-rel)
```
Function.

A strongly connected relation means that (x R y) or (y R x)
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L151-L163">Source</a></sub></p>

## <a name="relations/is-symmetric">`is-symmetric`</a><a name="relations/is-symmetric"></a>
``` clojure

(is-symmetric gen hb-rel)
```
Function.

A symmetric relation means that (a R b) => (b R a).
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L20-L33">Source</a></sub></p>

## <a name="relations/is-transitive">`is-transitive`</a><a name="relations/is-transitive"></a>
``` clojure

(is-transitive gen hb-rel)
```
Function.

hb-rel is a binary function which returns a `heavy-bool`
  `gen` is a collection
  `hb-rel` is a binary function returning a heavy-bool
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L35-L50">Source</a></sub></p>

## <a name="relations/lift-relation">`lift-relation`</a><a name="relations/lift-relation"></a>
``` clojure

(lift-relation rel)
```
Function.

Accepts a relation, a binary predicate returning a boolean,
  and returns a binary predicate returning a heavy-boolean.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L165-L170">Source</a></sub></p>
