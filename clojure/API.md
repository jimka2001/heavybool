# Table of contents
-  [`heavy-bool`](#heavy-bool)  - A <code>heavy-bool</code> is a pair <code>[bool reason]</code>, where <code>bool</code> is a truth value usually <code>true</code> or <code>false</code>, but may be any clojure truthy or falsey value.
    -  [`+and`](#heavy-bool/+and) - Logical AND of heavy-bools which evaluates to a <code>heavy-bool</code>.
    -  [`+annotate`](#heavy-bool/+annotate) - Add key/value pairs as annotation to a <code>heavy-bool</code>.
    -  [`+annotate-false`](#heavy-bool/+annotate-false) - Add key/value pairs as annotation to a false <code>heavy-bool</code>.
    -  [`+annotate-true`](#heavy-bool/+annotate-true) - Add key/value pairs as annotation to a true <code>heavy-bool</code>.
    -  [`+assert`](#heavy-bool/+assert) - Assert that the given <code>heavy-bool</code> object is logically true.
    -  [`+bool`](#heavy-bool/+bool) - convert a <code>heavy-bool</code> to explictly <code>true</code> or <code>false</code>.
    -  [`+conj`](#heavy-bool/+conj) - Conjoin an additional item to the reason list.
    -  [`+exists`](#heavy-bool/+exists) - Existential quantifier syntax.
    -  [`+exists-impl`](#heavy-bool/+exists-impl) - Function version of <code>+exists</code>.
    -  [`+false`](#heavy-bool/+false) - Standard false heavy-bool value.
    -  [`+forall`](#heavy-bool/+forall) - Universal quantifier syntax.
    -  [`+forall-impl`](#heavy-bool/+forall-impl) - Functional version of <code>+forall</code>.
    -  [`+heavy-bool`](#heavy-bool/+heavy-bool) - Constructor (factor function) for <code>heavy-bool</code>.
    -  [`+if`](#heavy-bool/+if) - <code>heavy-bool</code> version of <code>if</code>.
    -  [`+iff`](#heavy-bool/+iff) - a ==> b and b ==> a.
    -  [`+implied-by`](#heavy-bool/+implied-by) - Determine whether heavy-bool <code>a</code> logically implies <code>heavy-bool</code> <code>b</code>.
    -  [`+implies`](#heavy-bool/+implies) - Determine whether<code> heavy-bool</code> <code>a</code> logically implies <code>heavy-bool</code> <code>b</code>.
    -  [`+not`](#heavy-bool/+not) - logically negate the given <code>heavy-bool</code>.
    -  [`+or`](#heavy-bool/+or) - Logical OR of heavy-bools which evaluates to a <code>heavy-bool</code>.
    -  [`+tag`](#heavy-bool/+tag) - Conjoin the given key paired with the Boolean value of the given <code>heavy-bool</code>.
    -  [`+true`](#heavy-bool/+true) - Standard true heavy-bool value.
    -  [`assert-heavy-bool`](#heavy-bool/assert-heavy-bool) - Assert that a given object is a <code>heavy-bool</code>.
    -  [`ensure-heavy-bool`](#heavy-bool/ensure-heavy-bool) - test whether the given object is either already a <code>heavy-bool</code> or explicitly <code>true</code> or <code>false</code>.
    -  [`find-reason`](#heavy-bool/find-reason) - Search the reasons (each a map) within a <code>heavy-bool</code> for the first one that has the given key.
    -  [`find-witness`](#heavy-bool/find-witness) - Find the <code>:witness</code> which caused the existential quantifier to succeed or which caused the universal quantifier to fail.
    -  [`heavy-bool?`](#heavy-bool/heavy-bool?) - Predicate returning <code>true</code> if the given object is a <code>heavy-bool</code>, <code>false</code> otherwise.
    -  [`with-heavy-bool`](#heavy-bool/with-heavy-bool) - Macro which rebinds the named variable to a value ensured to be a <code>heavy-bool</code>.
-  [`util`](#util) 
    -  [`*time-out*`](#util/*time-out*) - Dynamic variable controling the default number of milliseconds is used as the time-out used by <code>testing-with-timeout</code>.
    -  [`almost-equal`](#util/almost-equal) - Returns a binary function which can be used to test whether two given arguments are within a given tolerance of each other.
    -  [`almost-equal-seq`](#util/almost-equal-seq) - Returns a binary function which can be used to test whether two given sequence arguments are element-wise within a given tolerance of each other.
    -  [`call-with-timeout`](#util/call-with-timeout) - Call the given 0-arty function in a separate thread (via <code>future</code>) and dereference it with <code>timeout-ms</code> as the time-out.
    -  [`find-if`](#util/find-if) - Find the first element in the sequence which makes the predicate true.
    -  [`first-st`](#util/first-st) - Return the first element of a collection which causes the <code>body</code> to be true.
    -  [`gcd`](#util/gcd) - Euclidean algorithm to compute greatest common divisor.
    -  [`member`](#util/member) - Determines whether the given target is an element of the given sequence (or given set).
    -  [`power`](#util/power) - compute the base <code>b</code> raised to the power <code>p</code>.
    -  [`power-set`](#util/power-set) - Given a set of items, <code>base-set</code> return a set containing all subsets of <code>base-set</code>.
    -  [`re-chunk`](#util/re-chunk) - Given a lazy sequence, change the chunking buffer size to n.
    -  [`tails`](#util/tails) - Return a lazy list of tails of the given collection.
    -  [`testing-with-timeout`](#util/testing-with-timeout) - Similar to the <code>clojure.test</code> macro <code>testing</code> but limits a time-out to <code>*time-out*</code>.
    -  [`time-call`](#util/time-call) - Evaluates thunk and returns a 2-vector [value elapsed-time] where value is the return value of the thunk and elapsed-time is the number of nano it took evaluating the function.
    -  [`type-check`](#util/type-check) - Evaluates to the value of <code>x</code> but with a pre-condition that (f x) is true.

-----
# <a name="heavy-bool">heavy-bool</a>


A [[`heavy-bool`](#heavy-bool)](#heavy-bool) is a pair `[bool reason]`, where `bool` is a truth value
  usually `true` or `false`, but may be any clojure truthy or falsey value.
  `reason` is a list of maps with keys such as `:witness`, `:bool`, and
  `:predicate` etc.  A [[`heavy-bool`](#heavy-bool)](#heavy-bool) answers a predicate question with either
  yes-because or no-because




## <a name="heavy-bool/+and">`+and`</a><a name="heavy-bool/+and"></a>
``` clojure

(+and & rest)
```
Macro.

Logical AND of heavy-bools which evaluates to a [`heavy-bool`](#heavy-bool).
  Expands to code which evaluates to the left-most `heavy-false` value
  in the argument list, otherwise evaluates to the right-most
  value.  If the argument list is empty, evaluates explicitly to
  [`+true`](#heavy-bool/+true)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L109-L124">Source</a></sub></p>

## <a name="heavy-bool/+annotate">`+annotate`</a><a name="heavy-bool/+annotate"></a>
``` clojure

(+annotate heavy-bool & {:as key-vals})
```
Function.

Add key/value pairs as annotation to a [[`heavy-bool`](#heavy-bool)](#heavy-bool).
  Eg. `(+annotate hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given [[`heavy-bool`](#heavy-bool)](#heavy-bool)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L164-L170">Source</a></sub></p>

## <a name="heavy-bool/+annotate-false">`+annotate-false`</a><a name="heavy-bool/+annotate-false"></a>
``` clojure

(+annotate-false heavy-bool & {:as key-vals})
```
Function.

Add key/value pairs as annotation to a false [[`heavy-bool`](#heavy-bool)](#heavy-bool).
  Eg. `(+annotate-true hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given [[`heavy-bool`](#heavy-bool)](#heavy-bool) if and only if it has false semantics.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L182-L190">Source</a></sub></p>

## <a name="heavy-bool/+annotate-true">`+annotate-true`</a><a name="heavy-bool/+annotate-true"></a>
``` clojure

(+annotate-true heavy-bool & {:as key-vals})
```
Function.

Add key/value pairs as annotation to a true [[`heavy-bool`](#heavy-bool)](#heavy-bool).
  Eg. `(+annotate-true hb :x x :y y)`
  to add `{:x x :y y}` as annotation on the given [[`heavy-bool`](#heavy-bool)](#heavy-bool) if and only if it has true semantics.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L172-L180">Source</a></sub></p>

## <a name="heavy-bool/+assert">`+assert`</a><a name="heavy-bool/+assert"></a>
``` clojure

(+assert [bool reason :as hb])
```
Function.

Assert that the given [`heavy-bool`](#heavy-bool) object is logically true
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L295-L300">Source</a></sub></p>

## <a name="heavy-bool/+bool">`+bool`</a><a name="heavy-bool/+bool"></a>
``` clojure

(+bool hb)
```
Function.

convert a [`heavy-bool`](#heavy-bool) to explictly `true` or `false`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L42-L46">Source</a></sub></p>

## <a name="heavy-bool/+conj">`+conj`</a><a name="heavy-bool/+conj"></a>
``` clojure

(+conj hb item)
```
Function.

Conjoin an additional item to the reason list
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L55-L62">Source</a></sub></p>

## <a name="heavy-bool/+exists">`+exists`</a><a name="heavy-bool/+exists"></a>
``` clojure

(+exists [v coll & others :as var-coll] & body)
```
Macro.

Existential quantifier syntax.  `body` is expected to evaluate
  to a heavy-bool.  The syntax is similar to `for` and `doseq`
  with `:let` and `:when` modifiers being supported but not `:while`
  `:when` can be followed by a boolean or a [`heavy-bool`](#heavy-bool).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L277-L284">Source</a></sub></p>

## <a name="heavy-bool/+exists-impl">`+exists-impl`</a><a name="heavy-bool/+exists-impl"></a>
``` clojure

(+exists-impl tag f coll)
```
Function.

Function version of [`+exists`](#heavy-bool/+exists).
  Traverses the given collection until 1) either an item is found
  which is `heavy-true` and return it (with a new reason conjoined),
  or 2) else returns explicitly [`+false`](#heavy-bool/+false).
  If some value in the collection causes the predicate to return
  `heavy-true`, then a reason will be specified which provides
  the `:witness` value (the example) which caused the predicate
  to succeed.  The `:predicate` is also given in the reason.
  `f` is a function which either returns a boolean (explicitly `true` or `false`)
       or returns a [`heavy-bool`](#heavy-bool)
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L224-L240">Source</a></sub></p>

## <a name="heavy-bool/+false">`+false`</a><a name="heavy-bool/+false"></a>




Standard false heavy-bool value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L12-L12">Source</a></sub></p>

## <a name="heavy-bool/+forall">`+forall`</a><a name="heavy-bool/+forall"></a>
``` clojure

(+forall [v coll & others :as var-coll] & body)
```
Macro.

Universal quantifier syntax.  `body` is expected to evaluate
  to a heavy-bool.    The syntax is similar to `for` and `doseq`
  with `:let` and `:when` modifiers being supported but not `:while`.
  `:when` can be followed by a boolean or a [`heavy-bool`](#heavy-bool).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L286-L293">Source</a></sub></p>

## <a name="heavy-bool/+forall-impl">`+forall-impl`</a><a name="heavy-bool/+forall-impl"></a>
``` clojure

(+forall-impl tag f coll)
```
Function.

Functional version of [`+forall`](#heavy-bool/+forall).
  Traverses the given collection until 1) either an item is found
  which is `heavy-false` and return it (with a new reason conjoined),
  or 2) else [`+true`](#heavy-bool/+true) is returned.
  If some value in the collection causes the predicate to return
  `heavy-false`, then a reason will be specified which provides
  the `:witness` value (the counter-example) which caused the predicate
  to fail.  The `:predicate` is also given in the reason.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L200-L222">Source</a></sub></p>

## <a name="heavy-bool/+heavy-bool">`+heavy-bool`</a><a name="heavy-bool/+heavy-bool"></a>
``` clojure

(+heavy-bool arg & {:as key-vals})
```
Function.

Constructor (factor function) for [[[`heavy-bool`](#heavy-bool)](#heavy-bool)](#heavy-bool).
  convert `arg` to a [[[`heavy-bool`](#heavy-bool)](#heavy-bool)](#heavy-bool).
  `true` is converted to [`+true`](#heavy-bool/+true)
  `false` is converted to [`+false`](#heavy-bool/+false)
  If `arg` is already a [[[`heavy-bool`](#heavy-bool)](#heavy-bool)](#heavy-bool), no conversion occurs.
  In any case, if other key/value pairs are given, they are conjoined via [`+conj`](#heavy-bool/+conj)
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L64-L74">Source</a></sub></p>

## <a name="heavy-bool/+if">`+if`</a><a name="heavy-bool/+if"></a>
``` clojure

(+if cond consequent alternative)
```
Macro.

[[[`heavy-bool`](#heavy-bool)](#heavy-bool)](#heavy-bool) version of `if`.  The condition must
  evaluate to a [[[`heavy-bool`](#heavy-bool)](#heavy-bool)](#heavy-bool).  Either the consequent or
  alternative will be evaluated depending on the [[[`heavy-bool`](#heavy-bool)](#heavy-bool)](#heavy-bool)
  value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L99-L107">Source</a></sub></p>

## <a name="heavy-bool/+iff">`+iff`</a><a name="heavy-bool/+iff"></a>
``` clojure

(+iff a b)
```
Function.

a ==> b and b ==> a
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L157-L161">Source</a></sub></p>

## <a name="heavy-bool/+implied-by">`+implied-by`</a><a name="heavy-bool/+implied-by"></a>
``` clojure

(+implied-by b a)
```
Macro.

Determine whether heavy-bool `a` logically implies [`heavy-bool`](#heavy-bool) `b`.
  `a` is not evaluated unless `b` is `heavy-false`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L150-L155">Source</a></sub></p>

## <a name="heavy-bool/+implies">`+implies`</a><a name="heavy-bool/+implies"></a>
``` clojure

(+implies a b)
```
Macro.

Determine whether` heavy-bool` `a` logically implies [`heavy-bool`](#heavy-bool) `b`.
  `b` is not evaluated unless `a` is `heavy-true`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L143-L148">Source</a></sub></p>

## <a name="heavy-bool/+not">`+not`</a><a name="heavy-bool/+not"></a>
``` clojure

(+not hb)
```
Function.

logically negate the given [`heavy-bool`](#heavy-bool)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L48-L53">Source</a></sub></p>

## <a name="heavy-bool/+or">`+or`</a><a name="heavy-bool/+or"></a>
``` clojure

(+or & rest)
```
Macro.

Logical OR of heavy-bools which evaluates to a [`heavy-bool`](#heavy-bool).
  Expands to code which evaluates to the left-most `heavy-true` value
  in the argument list, otherwise evaluates to the left-most
  value.  If the argument list is empty, evaluates explicitly to
  [`+false`](#heavy-bool/+false)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L126-L141">Source</a></sub></p>

## <a name="heavy-bool/+tag">`+tag`</a><a name="heavy-bool/+tag"></a>
``` clojure

(+tag heavy-bool key)
```
Function.

Conjoin the given key paired with the Boolean value of the given [`heavy-bool`](#heavy-bool)
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L192-L198">Source</a></sub></p>

## <a name="heavy-bool/+true">`+true`</a><a name="heavy-bool/+true"></a>




Standard true heavy-bool value.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L11-L11">Source</a></sub></p>

## <a name="heavy-bool/assert-heavy-bool">`assert-heavy-bool`</a><a name="heavy-bool/assert-heavy-bool"></a>
``` clojure

(assert-heavy-bool hb)
```
Function.

Assert that a given object is a [`heavy-bool`](#heavy-bool).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L243-L247">Source</a></sub></p>

## <a name="heavy-bool/ensure-heavy-bool">`ensure-heavy-bool`</a><a name="heavy-bool/ensure-heavy-bool"></a>
``` clojure

(ensure-heavy-bool hb)
```
Function.

test whether the given object is either already a [`heavy-bool`](#heavy-bool)
  or explicitly `true` or `false`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L23-L34">Source</a></sub></p>

## <a name="heavy-bool/find-reason">`find-reason`</a><a name="heavy-bool/find-reason"></a>
``` clojure

(find-reason hb key)
```
Function.

Search the reasons (each a map) within a [`heavy-bool`](#heavy-bool) for the first
  one that has the given key.  When found, return the value of the key.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L76-L90">Source</a></sub></p>

## <a name="heavy-bool/find-witness">`find-witness`</a><a name="heavy-bool/find-witness"></a>
``` clojure

(find-witness hb)
```
Function.

Find the `:witness` which caused the existential quantifier to succeed or
  which caused the universal quantifier to fail
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L92-L96">Source</a></sub></p>

## <a name="heavy-bool/heavy-bool?">`heavy-bool?`</a><a name="heavy-bool/heavy-bool?"></a>
``` clojure

(heavy-bool? heavy-bool)
```
Function.

Predicate returning `true` if the given object is a [`heavy-bool`](#heavy-bool), `false` otherwise.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L14-L21">Source</a></sub></p>

## <a name="heavy-bool/with-heavy-bool">`with-heavy-bool`</a><a name="heavy-bool/with-heavy-bool"></a>
``` clojure

(with-heavy-bool hb & body)
```
Macro.

Macro which rebinds the named variable to a value ensured to be a [`heavy-bool`](#heavy-bool).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/heavy_bool.clj#L36-L40">Source</a></sub></p>

-----
# <a name="util">util</a>






## <a name="util/*time-out*">`*time-out*`</a><a name="util/*time-out*"></a>




Dynamic variable controling the default number of milliseconds
  is used as the time-out used by [`testing-with-timeout`](#util/testing-with-timeout).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L88-L91">Source</a></sub></p>

## <a name="util/almost-equal">`almost-equal`</a><a name="util/almost-equal"></a>
``` clojure

(almost-equal tolerance)
```
Function.

Returns a binary function which can be used to test whether two
  given arguments are within a given tolerance of each other.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L36-L45">Source</a></sub></p>

## <a name="util/almost-equal-seq">`almost-equal-seq`</a><a name="util/almost-equal-seq"></a>
``` clojure

(almost-equal-seq tolerance)
```
Function.

Returns a binary function which can be used to test whether two
  given sequence arguments are element-wise within a given tolerance of each other.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L47-L55">Source</a></sub></p>

## <a name="util/call-with-timeout">`call-with-timeout`</a><a name="util/call-with-timeout"></a>
``` clojure

(call-with-timeout timeout-ms f)
```
Function.

Call the given 0-arty function in a separate thread (via `future`)
  and dereference it with `timeout-ms` as the time-out.
  This function is intended to be called withint a `clojure.test` test
  case (defined by `deftest`). If such a test times out, a test failure
  is registered, by a side-effecing call to `(is ...)`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L93-L111">Source</a></sub></p>

## <a name="util/find-if">`find-if`</a><a name="util/find-if"></a>
``` clojure

(find-if f col)
```
Function.

Find the first element in the sequence which makes the predicate true.
  If such an item is found, a singleton list of the item is returned,
  otherwise nil is returned.
  
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L68-L78">Source</a></sub></p>

## <a name="util/first-st">`first-st`</a><a name="util/first-st"></a>
``` clojure

(first-st var col & body)
```
Macro.

Return the first element of a collection which causes the `body`
  to be true.  [`find-if`](#util/find-if) is used to attempt prevent the expression
  from evaluating more times than necessary.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L81-L86">Source</a></sub></p>

## <a name="util/gcd">`gcd`</a><a name="util/gcd"></a>
``` clojure

(gcd a b)
```
Function.

Euclidean algorithm to compute greatest common divisor
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L163-L170">Source</a></sub></p>

## <a name="util/member">`member`</a><a name="util/member"></a>
``` clojure

(member target items)
```
Function.

Determines whether the given target is an element of the given sequence (or given set).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L23-L34">Source</a></sub></p>

## <a name="util/power">`power`</a><a name="util/power"></a>
``` clojure

(power b p)
```
Function.

compute the base `b` raised to the power `p`
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L142-L161">Source</a></sub></p>

## <a name="util/power-set">`power-set`</a><a name="util/power-set"></a>
``` clojure

(power-set base-set)
```
Function.

Given a set of items, `base-set` return a set containing
  all subsets of `base-set`.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L130-L141">Source</a></sub></p>

## <a name="util/re-chunk">`re-chunk`</a><a name="util/re-chunk"></a>
``` clojure

(re-chunk n xs)
```
Function.

Given a lazy sequence, change the chunking buffer size to n.
  This code was taken directory from
  https://clojuredocs.org/clojure.core/chunk
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L57-L66">Source</a></sub></p>

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

Similar to the `clojure.test` macro `testing` but limits
  a time-out to [`*time-out*`](#util/*time-out*).
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L113-L120">Source</a></sub></p>

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

Evaluates to the value of `x` but with a pre-condition
  that (f x) is true.
<p><sub><a href="https://github.com/jimka2001/heavybool/blob/main/clojure/src/util.clj#L122-L128">Source</a></sub></p>
