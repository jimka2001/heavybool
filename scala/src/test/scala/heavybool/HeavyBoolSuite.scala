package heavybool

import adjuvant.MyFunSuite
import HeavyBool._
import HbImplicits._

class HeavyBoolSuite extends MyFunSuite {
  test("and") {
    forallM("n", LazyList.range(1,10,3)){
      (n: Int) => (n % 2 != 0).tag("forall")
    } &&
      existsM("n", LazyList.range(1,10,3)){
        (n: Int) => (n % 2 != 0).tag("exists")
      }
  }

  test("or") {
    forallM(  "n", LazyList.range(1,10,3)) {
      (n: Int) => (n % 2 != 0).tag("forall")
    } || existsM("n", LazyList.range(1,10,3)){ (n: Int) =>
      (n % 2 != 0).tag("exists")
    }}

  test("forall"){
    assert(HTrue == forallM("x", LazyList(1,2,3)){ (x:Int) =>
      (x>0).tag("works")
    })
    println(forallM("x", LazyList(1,2,3)){ (x:Int) =>
      (x>0).tag("works")
    })

    val result = forallM("x", LazyList(1,2,3)){ (x:Int) =>
      (x>1).tag("works")
    }
    assert(result.toBoolean == false)
    assert(!result)
    assert(result.witness == Some(1))
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
