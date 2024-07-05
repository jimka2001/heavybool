package heavybool


import HeavyBool.Reason
import cats.Foldable


sealed abstract class HeavyBool(val because:Reason) {
  override def toString:String = locally{
    val prefix:String = toBoolean.toString
    val reasoning:List[String] = for{ m <- because
    } yield locally{
      val listOfKeys = (for{(k,v)<- m} yield s"$k->$v").toList

      if (1 == listOfKeys.size)
        listOfKeys.head
      else
        listOfKeys.mkString("(",", ",")")
    }
    prefix + reasoning.mkString("[", "; ", "]")
  }

  lazy val witness:Option[Any] = {
    because.find(m=>m.contains("witness")) match {
      case None => None
      case Some(m) => m.get("witness")
    }
  }

  def findWitness(tag:String):Option[Any] = {
    because.find(m=> (m.contains("witness") && m.contains("tag") && m("tag") == tag)) match {
      case None => None
      case Some(m) => m.get("witness")
    }
  }

  val toBoolean: Boolean = {
    this match {
      case HeavyTrue(_) => true
      case HeavyFalse(_) => false
    }
  }

  // logical OR between to HeavyBool objects, 
  // `that` is only evaluated if this is HeavyFalse.
  def ||(that: => HeavyBool):HeavyBool = {
    this match {
      case HeavyTrue(_) => this
      case HeavyFalse(_) => that
    }
  }


  // logical AND between to HeavyBool objects, 
  // `that` is only evaluated if this is HeavyTrue
  def &&(that: => HeavyBool):HeavyBool = {
    this match {
      case HeavyTrue(_) => that
      case HeavyFalse(_) => this
    }
  }

  def unary_! : HeavyBool = {
    this match {
      case HeavyTrue(str) => HeavyFalse(str)
      case HeavyFalse(str) => HeavyTrue(str)
    }
  }

  // implies:   this ==> that
  def ==>(that: => HeavyBool): HeavyBool = {
    !this || that
  }

  // implied by:   this <== that
  def <==(that: => HeavyBool): HeavyBool = {
    this || !that
  }

  def ++(any: Map[String,Any]): HeavyBool = {
    this match {
      case HeavyTrue(because) => HeavyTrue(any :: because)
      case HeavyFalse(because) => HeavyFalse(any :: because)
    }
  }

  def +| (reason:String): HeavyBool = this ++ Map("reason" -> reason)

  def tag(reason:String): HeavyBool = {
    this ++ Map(
      reason -> this.toBoolean
    )
  }

  def conjTrue(another: Map[String,Any]): HeavyBool = {
    this match {
      case HeavyTrue(_) => this ++ another
      case HeavyFalse(_) => this
    }
  }

  def conjFalse(another: Map[String,Any]): HeavyBool = {
    this match {
      case HeavyTrue(_) => this
      case HeavyFalse(_) => this ++ another
    }
  }
}

case class HeavyTrue(override val because: Reason) extends HeavyBool(because) {}

case class HeavyFalse(override val because: Reason) extends HeavyBool(because) {}

object HeavyBool {
  type Reason = List[Map[String, Any]]
  val HTrue = HeavyTrue(List())
  val HFalse = HeavyFalse(List())

  def apply(x:Boolean):HeavyBool = {
    if (x)
      HTrue
    else
      HFalse
  }

  def apply(test:Boolean, because:Reason):HeavyBool = {
    if (test)
      HeavyTrue(because)
    else
      HeavyFalse(because)
  }

  // this forallM declaration takes care of call-sites which specify a Range
  // as items.   the cats library is not able ot match the forallM[T, C[_]]
  // types for range because of some sort of type-arity kind-ness problem
  // that I don't understand.  However, declaring this method, avoids the problem.
  def forallM(tag:String, items: Range)(p: Int => HeavyBool): HeavyBool = {
    forallM[Int,LazyList](tag, items.to(LazyList))(p)
  }

  def forallM[T, C[_]:Foldable](tag:String, items: C[T])( p: T => HeavyBool): HeavyBool = {
    import cats._
    import cats.syntax.all._
    import HbImplicits._

    def folder(_hb:HeavyBool, item:T):Either[HeavyBool,HeavyBool] = {
      val hb = p(item)
      if (hb)
        Right(HTrue) // not yet finished
      else
        Left(hb ++ Map("witness" -> item,
                       "tag" -> tag)) // finished
    }

    items.foldM(HTrue:HeavyBool)(folder).merge
  }

  // this existsM declaration takes care of call-sites which specify a Range
  // as items.   the cats library is not able ot match the existsM[T, C[_]]
  // types for range because of some sort of type-arity kind-ness problem
  // that I don't understand.  However, declaring this method, avoids the problem.
  def existsM(tag:String, items: Range)(p: Int => HeavyBool): HeavyBool = {
    existsM[Int,LazyList](tag, items.to(LazyList))(p)
  }

  def existsM[T, C[_]:Foldable](tag:String, items: C[T])(p: T => HeavyBool): HeavyBool = {
    !(forallM[T,C](tag, items)(x => !(p(x))))
  }

  def assertM(a: HeavyBool):Unit = {
    a match {
      case HeavyTrue(_) => ()
      case HeavyFalse(str) => throw new java.lang.AssertionError(str)
    }
  }
  def main(argv:Array[String]):Unit = {}
}

