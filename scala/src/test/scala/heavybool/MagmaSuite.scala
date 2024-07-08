package heavybool

import adjuvant.MyFunSuite
import HbImplicits._
import heavybool.Magma.cayleyTable
import heavybool.examples.DynMagma

class MagmaSuite extends MyFunSuite {
  def testCayleyTables(n: Int): Unit = {
    import heavybool.Magma.{allUnitalCayleyTables, cayleyTable, genLazyFinite}
    for {add <- allUnitalCayleyTables(n)
         str = cayleyTable(genLazyFinite(n - 1), add)
         } println(str)
  }

  test("Cayley Tables") {
    testCayleyTables(2)
    testCayleyTables(3)
  }


  test("count groups") {
    import heavybool.Magma.{countGroups}
    countGroups(2)
    countGroups(3)
    //countGroups(4)
  }

  test("find groups") {
    import heavybool.Magma.{findGroupsM}
    findGroupsM(2)
    findGroupsM(3)
    findGroupsM(4)
  }

  class Test32a extends Magma[Int, LazyList] {
    def member(a: Int): HeavyBool = HeavyBool(0 <= a && a < 10)

    def op(a: Int, b: Int): Int = a - b

    def gen(): LazyList[Int] = (0 to 10).to(LazyList)
  }

  class Test32b extends Test32a {
    override def op(a: Int, b: Int): Int = a + b
  }

  val hbc: DynMagma[Int, LazyList] = DynMagma[Int, LazyList](
    () => (0 to 10).to(LazyList),
    (a: Int, b: Int) => a - b,
    (a: Int) => HeavyBool(0 <= a && a < 10)
    )
  val hbd: DynMagma[Int, LazyList] = DynMagma[Int, LazyList](
    () => (0 to 10).to(LazyList),
    (a: Int, b: Int) => a + b,
    (a: Int) => HeavyBool(0 <= a && a < 10)
    )
  test("is associative") {

    val hba = new Test32a().isAssociative()
    assert(!hba)

    val hbb = new Test32b().isAssociative()
    assert(hbb)
    assert(!hbc.isAssociative())
    assert(hbd.isAssociative())
  }

  test("is commutative") {
    val hba = new Test32a().isCommutative()
    assert(!hba)
    val hbb = new Test32b().isCommutative()
    assert(hbb)

    assert(!hbc.isCommutative())

    assert(hbd.isCommutative())
  }

  test("is identity"){
    assert(None == new Test32a().findIdentity())
    assert(Some(0) == new Test32b().findIdentity())
  }

  test("inverse"){
    assert(None == new Test32a().findInverse(2))
    assert(None == new Test32b().findInverse(2))
  }

  class Klein4 extends Magma[String, List]{
    val elements = List("e", "A", "B", "C")
    def member(a: String): HeavyBool = HeavyBool(elements.contains(a))

    def op(x: String, y: String):String = {
      if ("e" == x)
        y
      else if ("e" == y)
        x
      else if (x == y)
        x // "e"
      else {
        // find the element of "A", "B", "C" which is not x and is not y
        elements.find( z => z != "e" && z != x && z != y).get
      }
    }

    def gen(): List[String] = elements
  }

  class NotKlein4 extends Klein4 {
    override op(x: String, y: String): String = {
      if ("e" == x)
        y
      else if ("e" == y)
        x
      else if (x == y)
        x // "e"
      else {
        // find the element of "A", "B", "C" which is not x and is not y
        elements.find(z => z != "e" && z != x && z != y).get
      }
    }
  }
  test("klein group"){
    val k4 = new Klein4()
    println(k4.isGroup())
    assert(k4.isGroup())
  }
}
