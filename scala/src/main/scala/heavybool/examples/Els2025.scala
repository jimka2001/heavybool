package heavybool.examples

object Els2025 {
  val M = Seq(1,2,3,4,5)
  val N = Seq("a", "b", "c", "")

  def opM(a:Int, b:Int):Int = {
    a - b
  }

  def opN(a:String, b:String): String = {
    a ++ b
  }

  def testCommutative[T](M:Seq[T], op:(T,T)=>T):Boolean = {
    M.forall{(a) =>
      M.forall{(b) =>
        op(a,b) == op(b,a)}}
  }

  def findNonCommutative[T](M:Seq[T], op:(T,T)=>T):Option[T] = {
    M.find{(a) =>
      M.exists{(b) =>
        op(a,b) != op(b,a)}}
  }

  def findCounterExamples[T](M:Seq[T], op:(T,T)=>T):LazyList[(T,T)] = {
    for{a <- M.to(LazyList)
        b <- M.to(LazyList)
        if op(a,b)!= op(b,a)
        } yield (a,b)
  }
  def main(argv:Array[String]):Unit = {
    testCommutative(M,opM)
  }

}
