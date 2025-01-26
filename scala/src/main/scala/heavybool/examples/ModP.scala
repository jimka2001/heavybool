package heavybool

import HeavyBool._
import cats.Foldable

abstract class ModP(p: Int) extends Magma[Int, List] {
  override def toString: String = s"ModP($p)"

  override def gen(): List[Int] = Magma.genListFinite(p - 1)

  override def equiv(a: Int, b: Int): HeavyBool = {
    if (a == b)
      HTrue +| s"$a equiv $b"
    else
      HFalse +| s"$a not equiv $b"
  }.tag(s"equiv mod $p")

  override def member(a: Int): HeavyBool = {
    if (a < 0)
      HFalse +| s"$a is not member because $a < 0"
    else if (a >= p)
      HFalse +| s"$a is not a member because $a >= $p"
    else
      HTrue +| s"0 <= $a < $p"
  }.tag("member")
}

class AdditionModP(p: Int) extends ModP(p) {
  override def toString: String = s"AdditionModP($p)"

  override def op(a: Int, b: Int): Int = (a + b) % p

  def suggestInverse(a:Int):Option[Int] = {
    Some(0)
  }
}

class MultiplicationModP(p: Int) extends ModP(p) {
  override def toString: String = s"MultiplicationModP($p)"

  override def gen(): List[Int] = {
    super.gen().filter { (a:Int) => a != 0 }
  }

  override def member(a: Int): HeavyBool = {
    if (a <= 0)
      HFalse +| s"$a <= 0"
    else
      super.member(a)
  }.tag("member")

  override def op(a: Int, b: Int): Int = (a * b) % p
}
