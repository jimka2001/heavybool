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
    -  [`is-antisymmetric`](#relations/is-antisymmetric) - Test for antisymmetric relation: ((a R b) and (b R a)) => (a = b).
    -  [`is-asymmetric`](#relations/is-asymmetric) - Test for asymmetric relation.
    -  [`is-equivalence`](#relations/is-equivalence)
    -  [`is-irreflexive`](#relations/is-irreflexive)
    -  [`is-reflexive`](#relations/is-reflexive)
    -  [`is-strict-partial-order`](#relations/is-strict-partial-order) - A strict partial order is irreflexive, transitive, and asymmetric.
    -  [`is-symmetric`](#relations/is-symmetric)
    -  [`is-transitive`](#relations/is-transitive) - rel is a binary function which returns a Boolean.

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
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L22-L28">Source</a></sub></p>

## <a name="magma/find-identity">`find-identity`</a><a name="magma/find-identity"></a>
``` clojure

(find-identity coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L67-L73">Source</a></sub></p>

## <a name="magma/has-inverses">`has-inverses`</a><a name="magma/has-inverses"></a>
``` clojure

(has-inverses coll * ident invert member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L97-L117">Source</a></sub></p>

## <a name="magma/is-associative">`is-associative`</a><a name="magma/is-associative"></a>
``` clojure

(is-associative coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L30-L41">Source</a></sub></p>

## <a name="magma/is-closed">`is-closed`</a><a name="magma/is-closed"></a>
``` clojure

(is-closed coll * member)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L11-L20">Source</a></sub></p>

## <a name="magma/is-commutative">`is-commutative`</a><a name="magma/is-commutative"></a>
``` clojure

(is-commutative coll * equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L43-L53">Source</a></sub></p>

## <a name="magma/is-field">`is-field`</a><a name="magma/is-field"></a>
``` clojure

(is-field coll + * zero one +inv *inv member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L151-L175">Source</a></sub></p>

## <a name="magma/is-group">`is-group`</a><a name="magma/is-group"></a>
``` clojure

(is-group coll * ident invert member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L119-L128">Source</a></sub></p>

## <a name="magma/is-identity">`is-identity`</a><a name="magma/is-identity"></a>
``` clojure

(is-identity coll * ident equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L55-L65">Source</a></sub></p>

## <a name="magma/is-monoid">`is-monoid`</a><a name="magma/is-monoid"></a>
``` clojure

(is-monoid coll * ident member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L85-L95">Source</a></sub></p>

## <a name="magma/is-ring">`is-ring`</a><a name="magma/is-ring"></a>
``` clojure

(is-ring coll + * zero one +inv member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L130-L149">Source</a></sub></p>

## <a name="magma/is-semigroup">`is-semigroup`</a><a name="magma/is-semigroup"></a>
``` clojure

(is-semigroup coll * member equal)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/magma.clj#L75-L83">Source</a></sub></p>

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
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/mod_p.clj#L9-L47">Source</a></sub></p>

-----
# <a name="relations">relations</a>


Example usage of `heavy-bool`.
  This namespace defines several relations such as reflexive, symmetric, and antisymmetric.
  




## <a name="relations/is-antisymmetric">`is-antisymmetric`</a><a name="relations/is-antisymmetric"></a>
``` clojure

(is-antisymmetric gen rel)
```
Function.

Test for antisymmetric relation:
  ((a R b) and (b R a)) => (a = b)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L69-L82">Source</a></sub></p>

## <a name="relations/is-asymmetric">`is-asymmetric`</a><a name="relations/is-asymmetric"></a>
``` clojure

(is-asymmetric gen rel)
```
Function.

Test for asymmetric relation
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L55-L67">Source</a></sub></p>

## <a name="relations/is-equivalence">`is-equivalence`</a><a name="relations/is-equivalence"></a>
``` clojure

(is-equivalence gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L45-L52">Source</a></sub></p>

## <a name="relations/is-irreflexive">`is-irreflexive`</a><a name="relations/is-irreflexive"></a>
``` clojure

(is-irreflexive gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L84-L90">Source</a></sub></p>

## <a name="relations/is-reflexive">`is-reflexive`</a><a name="relations/is-reflexive"></a>
``` clojure

(is-reflexive gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L8-L15">Source</a></sub></p>

## <a name="relations/is-strict-partial-order">`is-strict-partial-order`</a><a name="relations/is-strict-partial-order"></a>
``` clojure

(is-strict-partial-order gen rel)
```
Function.

A strict partial order is irreflexive, transitive, and asymmetric.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L92-L101">Source</a></sub></p>

## <a name="relations/is-symmetric">`is-symmetric`</a><a name="relations/is-symmetric"></a>
``` clojure

(is-symmetric gen rel)
```
Function.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L17-L28">Source</a></sub></p>

## <a name="relations/is-transitive">`is-transitive`</a><a name="relations/is-transitive"></a>
``` clojure

(is-transitive gen rel)
```
Function.

rel is a binary function which returns a Boolean
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/examples/relations.clj#L30-L43">Source</a></sub></p>
