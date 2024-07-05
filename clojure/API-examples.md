# Table of contents
-  [`gaussian-int`](#gaussian-int)  - Example usage of <code>heavy-bool</code>.
    -  [`gaussian-int-mod-p`](#gaussian-int/gaussian-int-mod-p) - For a given integer return a map with the keys: :p :gen :op :equiv :mult :one :zero :add :add-inv :mult-inv :member These keys are the data necessary to make a call to the <code>is-field</code> function.
    -  [`gaussian?`](#gaussian-int/gaussian?) - Bolean predicate detecting whether the given value is a 2 element vector of integers.
-  [`magma`](#magma)  - Example usage of <code>heavy-bool</code>.
    -  [`default-equal`](#magma/default-equal) - The default equal predicate which returns a <code>heavy-bool</code> indicating whether the arguments <code>left</code> and <code>right</code> are equal to each other.
    -  [`find-identity`](#magma/find-identity) - Predicate returning <code>heavy-bool</code> indicating whether there exists an identity element in the given collection of elements.
    -  [`has-inverses`](#magma/has-inverses) - Predicate returning a <code>heavy-bool</code> to determine whether every element of <code>coll</code> has an inverse for the given <code>*</code> operation w.r.t.
    -  [`is-associative`](#magma/is-associative) - Predicate returning a <code>heavy-bool</code> indicating whether the given operation is associative on the given collection of elements.
    -  [`is-closed`](#magma/is-closed) - Predicate returning a <code>heavy-bool</code>, determining whether the given operation <code>*</code> is closed on the given collection <code>coll</code>.
    -  [`is-commutative`](#magma/is-commutative) - Predicate returning a <code>heavy-bool</code> indicating whether the given operation is commutative on the given collection of elements.
    -  [`is-field`](#magma/is-field) - Predicate returning a <code>heavy-bool</code> indicating whether the given collection forms a field under the given operations.
    -  [`is-group`](#magma/is-group) - Predicate returning a <code>heavy-bool</code> indicating whether the given <code>coll</code> is a group under the given operation <code>*</code> with identity <code>ident</code>.
    -  [`is-identity`](#magma/is-identity) - Predicate returning a <code>heavy-bool</code> indicating whether the proposed identity, <code>ident</code> is really a left and right identity on the given collection of elements.
    -  [`is-magma`](#magma/is-magma) - Predicate returning a <code>heavy-bool</code> indicating whether the given collection forms a magma under the given operation.
    -  [`is-monoid`](#magma/is-monoid) - Predicate returning a <code>heavy-bool</code> indicating whether the given collection forms a monoid under the given operation.
    -  [`is-ring`](#magma/is-ring) - Predicate returning a <code>heavy-bool</code> indicating whether the given collection forms a ring under the given operation.
    -  [`is-semigroup`](#magma/is-semigroup) - Predicate returning a <code>heavy-bool</code> indicating whether the given collection forms a semigroup under the given operation.
-  [`mod-p`](#mod-p)  - Example usage of <code>heavy-bool</code>.
    -  [`addition-mod-p`](#mod-p/addition-mod-p)
    -  [`multiplication-mod-p`](#mod-p/multiplication-mod-p)
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

For a given integer return a map with the keys:
    :p
    :gen
    :op
    :equiv
    :mult
    :one
    :zero
    :add
    :add-inv
    :mult-inv
    :member
  These keys are the data necessary to make a call to the `is-field` function.
  The values of each key is determined in a way so that if p is an odd prime
  of the form 4k-1, then the set of Gaussian integers modulo p is a field.
  Otherwise the set is not a field.
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/gaussian_int.clj#L16-L145">Source</a></sub></p>

## <a name="gaussian-int/gaussian?">`gaussian?`</a><a name="gaussian-int/gaussian?"></a>
``` clojure

(gaussian? g)
```
Function.

Bolean predicate detecting whether the given value is a 2 element vector of integers
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/gaussian_int.clj#L8-L14">Source</a></sub></p>

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

The default equal predicate which returns a `heavy-bool` indicating
  whether the arguments `left` and `right` are equal to each other
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L27-L36">Source</a></sub></p>

## <a name="magma/find-identity">`find-identity`</a><a name="magma/find-identity"></a>
``` clojure

(find-identity coll * equal)
```
Function.

Predicate returning `heavy-bool` indicating whether there exists an identity element
  in the given collection of elements.   If such an element is found, it may be extracted
  from the `heavy-bool` using `find-witness`.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `equal` -- equivalence predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L91-L103">Source</a></sub></p>

## <a name="magma/has-inverses">`has-inverses`</a><a name="magma/has-inverses"></a>
``` clojure

(has-inverses coll * ident invertible member equal)
```
Function.

Predicate returning a `heavy-bool` to determine whether every element
  of `coll` has an inverse for the given `*` operation w.r.t. the given `ident`.
    `coll` -- collections of object in the magma
    `*` -- binary function, takes two elements from `coll` and returns a new element
    `ident` -- the identity for the magma
    `invertible` -- function which takes an element from `coll` and returns a heavy-bool
                  indicating whether the given element is invertible.  If an element
                  is invertible, then `(find-witness hb)` should returns its inverse.
    `member` -- membership predicate, returning `heavy-bool`
    `equal` -- equality predicate, returning `heavy-bool`.
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L157-L186">Source</a></sub></p>

## <a name="magma/is-associative">`is-associative`</a><a name="magma/is-associative"></a>
``` clojure

(is-associative coll * equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given operation
  is associative on the given collection of elements.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `equal` -- equivalence predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L38-L54">Source</a></sub></p>

## <a name="magma/is-closed">`is-closed`</a><a name="magma/is-closed"></a>
``` clojure

(is-closed coll * member)
```
Function.

Predicate returning a `heavy-bool`, determining whether the given operation `*`
  is closed on the given collection `coll`.
  `coll` -- a collection of values representing a finite mathematical set.
  `*` -- a binary operator which accepts two elements of `coll`
  `member` -- membership predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L11-L25">Source</a></sub></p>

## <a name="magma/is-commutative">`is-commutative`</a><a name="magma/is-commutative"></a>
``` clojure

(is-commutative coll * equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given operation
  is commutative on the given collection of elements.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `equal` -- equivalence predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L56-L71">Source</a></sub></p>

## <a name="magma/is-field">`is-field`</a><a name="magma/is-field"></a>
``` clojure

(is-field coll + * zero one +invertible *invertible member equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given collection
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
  `equal` -- equality predicate, returning `heavy-bool`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L249-L294">Source</a></sub></p>

## <a name="magma/is-group">`is-group`</a><a name="magma/is-group"></a>
``` clojure

(is-group coll * ident invertible member equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given `coll` is a group
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
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L188-L210">Source</a></sub></p>

## <a name="magma/is-identity">`is-identity`</a><a name="magma/is-identity"></a>
``` clojure

(is-identity coll * ident equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the proposed identity, `ident`
  is really a left and right identity on the given collection of elements.
   `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `ident` -- the proposed identity element
   `equal` -- equivalence predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L73-L89">Source</a></sub></p>

## <a name="magma/is-magma">`is-magma`</a><a name="magma/is-magma"></a>
``` clojure

(is-magma coll * member)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given collection
   forms a magma under the given operation.
    A magma is a set which is closed under the operation
   `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `member` -- membership predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L107-L119">Source</a></sub></p>

## <a name="magma/is-monoid">`is-monoid`</a><a name="magma/is-monoid"></a>
``` clojure

(is-monoid coll * ident member equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given collection
   forms a monoid under the given operation.
   A monoid is a semigroup containing an identity for the given operation.
  `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `member` -- membership predicate returning a `heavy-bool`
   `equal` -- equivalence predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L138-L155">Source</a></sub></p>

## <a name="magma/is-ring">`is-ring`</a><a name="magma/is-ring"></a>
``` clojure

(is-ring coll + * zero one +invertible member equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given collection
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
  `equal` -- equality predicate, returning `heavy-bool`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L213-L247">Source</a></sub></p>

## <a name="magma/is-semigroup">`is-semigroup`</a><a name="magma/is-semigroup"></a>
``` clojure

(is-semigroup coll * member equal)
```
Function.

Predicate returning a `heavy-bool` indicating whether the given collection
   forms a semigroup under the given operation.
   A semigroup is a magma with an associative operation.
   `coll` -- a collection of values representing a finite mathematical set.
   `*` -- a binary operator which accepts two elements of `coll`
   `member` -- membership predicate returning a `heavy-bool`
   `equal` -- equivalence predicate returning a `heavy-bool`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L121-L136">Source</a></sub></p>

-----
# <a name="mod-p">mod-p</a>


Example usage of `heavy-bool`.
  This namespace implements an algebraic structure [`mod-p`](#mod-p).
  This structure is useful for testing the `examples.magma/is-group` function.
  




## <a name="mod-p/addition-mod-p">`addition-mod-p`</a><a name="mod-p/addition-mod-p"></a>
``` clojure

(addition-mod-p p)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/mod_p.clj#L53-L58">Source</a></sub></p>

## <a name="mod-p/multiplication-mod-p">`multiplication-mod-p`</a><a name="mod-p/multiplication-mod-p"></a>
``` clojure

(multiplication-mod-p p)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/mod_p.clj#L60-L65">Source</a></sub></p>

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
