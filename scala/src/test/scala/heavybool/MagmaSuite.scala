package heavybool

import adjuvant.MyFunSuite
import HbImplicits._
import heavybool.examples.DynMagma

class MagmaSuite extends MyFunSuite {
  def testCayleyTables(n:Int):Unit = {
    import heavybool.Magma.{allUnitalCayleyTables, cayleyTable, genLazyFinite}
    for {add <- allUnitalCayleyTables(n)
         str = cayleyTable(genLazyFinite(n-1), add)
         } println(str)
  }

  test("Cayley Tables"){
    testCayleyTables(2)
    testCayleyTables(3)
  }


  test("count groups"){
    import heavybool.Magma.{countGroups}
    countGroups(2)
    countGroups(3)
    //countGroups(4)
  }

  test("find groups"){
    import heavybool.Magma.{findGroupsM}
    findGroupsM(2)
    findGroupsM(3)
    findGroupsM(4)
  }

  test("is closed"){
    class Test32a extends Magma[Int,LazyList] {
      def member(a:Int):HeavyBool =  HeavyBool(0 <= a && a < 10)
      def op(a:Int, b:Int):Int = a - b
      def gen():LazyList[Int] = (0 to 10).to(LazyList)
    }
    val hba = new Test32a().isAssociative()
    assert(!hba)
    class Test32b extends Test32a {
      override def op(a:Int, b:Int):Int = a + b
    }
    val hbb = new Test32b().isAssociative()
    assert(hbb)

    val hbc:DynMagma[Int,LazyList] = DynMagma[Int,LazyList](
      () => (0 to 10).to(LazyList),
      (a:Int, b:Int) => a - b,
      (a:Int) =>  HeavyBool(0 <= a && a < 10)
      )
    assert(! hbc.isAssociative())

    val hbd:DynMagma[Int,LazyList] = DynMagma[Int,LazyList](
      () => (0 to 10).to(LazyList),
      (a:Int, b:Int) => a + b,
      (a:Int) =>  HeavyBool(0 <= a && a < 10)
      )
    assert( hbd.isAssociative())

  }
}
