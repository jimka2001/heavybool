package heavybool


import HeavyBool.Reason
import cats.Foldable

/**
 * Represents true-because or false-because.
 *
 * @param because a list of hash tables explaining the reasoning for the Boolean value
 */
sealed abstract class HeavyBool(val because:Reason) {
  // TODO refactor this class to use 
  // https://scastie.scala-lang.org/BalmungSan/bNwF2C8lToK0Onq30b62iQ/5
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

  /**
   * A value representing the witness value of an existential quantifier or
   *    the counterexample of a universal quantifier.
   */
  lazy val witness:Option[Any] = {
    because.find(m=>m.contains("witness")) match {
      case None => None
      case Some(m) => m.get("witness")
    }
  }

  /**
   * If this HeavyBool is a result of concentric existential or universal
   * quantifiers, then each may have left a witness tagged by a variable name.
   * This method finds the witness associated with a variable name (string).
   * @param tag
   * @return
   */
  def findWitness(tag:String):Option[Any] = {
    because.find(m=> (m.contains("witness") && m.contains("tag") && m("tag") == tag)) match {
      case None => None
      case Some(m) => m.get("witness")
    }
  }

  /**
   * Convert a HeavyBool to true or false
   */
  val toBoolean: Boolean = {
    this match {
      case HeavyTrue(_) => true
      case HeavyFalse(_) => false
    }
  }

  /**
   * logical OR between to HeavyBool objects,
   * `that` is only evaluated if this is HeavyFalse.
   *
   * @param that a call-by-name expression
   */
  def ||(that: => HeavyBool):HeavyBool = {
    this match {
      case HeavyTrue(_) => this
      case HeavyFalse(_) => that
    }
  }


  /**
   * logical AND between to HeavyBool objects,
   * `that` is only evaluated if `this` is HeavyTrue
   */
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

  /**
   * implies:   this `==>` that
   * `==>` uses a call-by-name argument because we want to
   * avoid that being evaluated if this is already false.
   */
  def ==>(that: => HeavyBool): HeavyBool = {
    !this || that
  }

  /**
   * implied by:   this `<==` that
   * `<==` uses a call-by-name argument because we want to
   * avoid that being evaluated if this is already false.
   */
  def <==(that: => HeavyBool): HeavyBool = {
    this || !that
  }

  /**
   * this `==>` that and also that `==>` this
   */
  def <==>(that: => HeavyBool): HeavyBool = {
    (this ==> that) && (this <== that)
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

  /**
   * This forallM declaration takes care of call-sites which specify a Range
   * as items.   The forallM[T, C[_]] function defined below does not work with
   * Range for the same reason foldM does not work with Range.
   * However, declaring forallM directly on (String, Range) avoids the problem.
   */
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

  /**
   * This existsM declaration takes care of call-sites which specify a Range
   * as items.   The existsM[T, C[_]] function defined below does not work with
   * Range for the same reason foldM does not work with Range.
   *  However, declaring existsM directly on (String, Range) avoids the problem.
   */
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

