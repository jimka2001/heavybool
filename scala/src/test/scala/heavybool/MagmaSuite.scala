package heavybool

import adjuvant.MyFunSuite

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
}
