package heavybool

import adjuvant.MyFunSuite
import heavybool.HeavyBool._
import heavybool.examples.Relations._

class RelationsSuite extends MyFunSuite {
  test("symmetric") {
    assert(existsM("a < 12", LazyList.range(1, 20)) { (a: Int) => HeavyBool(a < 12) }.toBoolean)
    assert(!isSymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isSymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isSymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  test("assymmetric") {
    // if a relates to b, then b does not relate to a
    assert(isAsymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isAsymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(!isAsymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  test("antisymmetric") {
    // if a relates to b, and b relates to a, then a == b
    assert(isAntisymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(isAntisymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isAntisymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(!isAntisymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a != b).toBoolean)
  }
  test("transitive") {
    assert(isTransitive(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(isTransitive(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isTransitive(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  test("reflexive") {
    assert(isReflexive(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(isReflexive(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(!isReflexive(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
  }
  test("irreflexive") {
    // no element relates to itself
    assert(!isIrreflexive(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(!isIrreflexive(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isIrreflexive(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
  }
  test("equivalence") {
    assert(!isEquivalence(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isEquivalence(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isEquivalence(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  test("partial order") {
    assert(isPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(!isPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(isPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(!isPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a != b).toBoolean)

  }
  test("strict partial order") {
    assert(isStrictPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isStrictPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(!isStrictPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(!isStrictPartialOrder(LazyList.range(1, 20), (a: Int, b: Int) => a != b).toBoolean)
  }
  test("connected") {
    assert(isConnected(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(isConnected(LazyList.range(1, 20), (a: Int, b: Int) => a > b).toBoolean)

    assert(isConnected(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isConnected(LazyList.range(1, 20), (a: Int, b: Int) => a >= b).toBoolean)

    assert(!isConnected(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(isConnected(LazyList.range(1, 20), (a: Int, b: Int) => a != b).toBoolean)
  }

  test("strongly connected") {
    assert(!isStronglyConnected(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isStronglyConnected(LazyList.range(1, 20), (a: Int, b: Int) => a > b).toBoolean)

    assert(isStronglyConnected(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isStronglyConnected(LazyList.range(1, 20), (a: Int, b: Int) => a >= b).toBoolean)

    assert(!isStronglyConnected(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(!isStronglyConnected(LazyList.range(1, 20), (a: Int, b: Int) => a != b).toBoolean)
  }

}
