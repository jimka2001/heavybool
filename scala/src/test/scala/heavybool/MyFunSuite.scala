package adjuvant
import org.scalatest.funsuite.AnyFunSuite

class MyFunSuite extends AnyFunSuite {
  import org.scalactic.source
  import org.scalatest.Tag

  val num_random_tests:Int = sys.env.get("CI_REGISTRY_IMAGE") match {
    case None => 1000   // if interactive
    case Some(_) => 100 // if in ci/cd pipeline
  }

  def printTime(time: Long): String = {
    def recur(time: Long, divisors: List[Int]): List[Int] = {
      divisors match {
        case Nil => Nil
        case d :: ds => (time / d).toInt :: recur(time / d, ds)
      }
    }

    val List(ns, us, ms, sec, min, hour) = recur(time, List(1000, 1000, 1000,
                                                            60, 60, 60))
    if (hour > 0)
      s"$hour hours $min min $sec sec"
    else if (min > 0)
      s"$min min $sec sec"
    else if (sec > 0)
      s"$sec sec $ms ms"
    else if (ms > 0)
      s"$ms ms"
    else if (us > 0)
      s"$us us"
    else
      s"$ns ns"
  }

  override def test(testName: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position):Unit = {
    super.test(testName,testTags : _*)(locally{
      val start = System.nanoTime()
      println("[ starting " + testName)
      var finished = false
      try{
        testFun
        finished = true
      }
      finally{
        val end = System.nanoTime()
        if (finished)
          println("] finished " + testName + ": " + printTime(end-start))
        else
          println("] aborted " + testName + ": " + printTime(end - start))
      }
    })(pos)
  }
}
