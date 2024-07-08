package heavybool

import adjuvant.MyFunSuite
import HeavyBool._
import HbImplicits._
import cats.Foldable


class HeavyBoolSuite extends MyFunSuite {
  test("and") {
    assert(
    forallM("n", Range(1,10,3)){
      (n: Int) => (n > 0).tag("forall")
    } &&
      existsM("n", Range(1,10,3)){
        (n: Int) => (n % 2 != 0).tag("exists")
      }
    )
  }

  test("or") {
    assert(
      forallM("n", (1 to 10 by 3)) {
        (n: Int) => (n % 2 != 0).tag("forall")
      } || existsM("n", (1 to 10 by 3)) { (n: Int) =>
        (n % 2 != 0).tag("exists")
      }
      )
  }

  test("forall") {
    assert(HTrue == forallM("x", List(1, 2, 3)) { (x: Int) =>
      (x > 0).tag("works")
    })

    val result = forallM("x", List(1, 2, 3)) { (x: Int) =>
      (x > 1).tag("works")
    }
    assert(result.toBoolean == false)
    assert(!result)
    assert(result.witness == Some(1)) // 1 is the first element that fails the forall
  }
  test("forall witness") {
    val hb_xy = forallM("x", (0 to 100)){x =>
      forallM("y", (0 to 100)){ y=>
        (x + y < 120)
      }
    }
    val x = hb_xy.findWitness("x") match {
      case Some(x:Int) => x
    }
    val y = hb_xy.findWitness("y") match {
      case Some(y:Int) => y
    }
    assert(x + y == 120)
  }

  test("exists") {
    assert(HFalse == existsM("x", List(1, 2, 3)) { (x: Int) =>
      (x > 10).tag("works")
    })

    val result = existsM("x", Seq(1, 2, 3)) { (x: Int) =>
      (x > 1).tag("works")
    }
    assert(result.toBoolean == true)
    assert(result)
    assert(result.witness == Some(2)) // 2 is the first element that succeeds the exists
  }

  test("exists witness") {
    // pythag triple
    val pt = existsM("a", (1 to 100)){
      a => existsM("b", (a+1 to 100)){
        b => existsM("c", (b+1 to 100)){
          c => a*a + b*b == c*c}
      }}
    val a = pt.findWitness("a") match {
      case Some(a:Int) => a
    }
    assert(a>0)
    val b = pt.findWitness("b") match {
      case Some(b:Int) => b
    }
    assert(b>0)
    val c = pt.findWitness("c") match {
      case Some(c:Int) => c
    }
    assert(c>0)

    assert(a*a + b*b == c*c)
  }

  test("logic") {
    val x = List(Map("reason" -> "x"))
    val y = List(Map("reason" -> "y"))
    assert((HeavyTrue(x) && HeavyTrue(y)) == HeavyTrue(y))
    assert((HeavyTrue(x) || HeavyTrue(y)) == HeavyTrue(x))
    assert((HeavyFalse(x) && HeavyFalse(y)) == HeavyFalse(x))
    assert((HeavyFalse(x) || HeavyFalse(y)) == HeavyFalse(y))
  }

  test("if"){
    assert(1 == (if (HTrue) 1 else 2))
    assert(2 == (if (HFalse) 1 else 2))
  }
}
