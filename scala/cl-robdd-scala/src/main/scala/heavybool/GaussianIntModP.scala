package heavybool
import HeavyBool._
import cats.Foldable

class GaussianIntModP(p: Int) extends Magma[(Int,Int), LazyList] {

  override def toString: String = s"GaussianModP($p)"
  val zero = (0,0)
  val one = (1,0)

  override def gen(): LazyList[(Int,Int)] = {
    def loop(u:Int,v:Int):LazyList[(Int,Int)] = {
      if (u==p)
        LazyList.empty
      else if (v==p)
        loop(u+1,0)
      else
        (u,v) #:: loop(u,v+1)
    }
    loop(0,0)
  }


  def mod(a:Int):Int = {
    ((a % p) + p) % p
  }

  def mod2(a:Int, b:Int):(Int,Int) = {
    (mod(a), mod(b))
  }

  def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    mod2(x + u, y + v)
  }

  def subtract(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    mod2(x - u, y - v)
  }

  def mult(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    mod2(u*x - v*y, u*y  + v*x)
  }

  def mult_inv(ab:(Int,Int)):Option[(Int,Int)] = {
    // a+bi has inverse (a - bi)/(a^2 + b^2) in the
    //   case that a^2 + b^2 is non-zero (modulo p)
    val (a,b) = ab
    val denom = mod(a*a + b*b)
    if (denom == 0)
      None
    else
      (1 until p).find(z => mod(z*denom) == 1)
        .map(z => mod2(a*z, -b*z))
  }

  def op(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    add(a,b)
  }

  override def equiv(a: (Int,Int), b: (Int,Int)): HeavyBool =
    if (a == b)
      HTrue ++ Map("reason" -> "a equiv b",
                   "a" -> a,
                   "b" -> b)

    else {
      HFalse ++ Map("reason" -> "a not equiv b",
                    "a" -> a,
                    "b" -> b)
    }

  override def member(a: (Int,Int)): HeavyBool = ({
    val (x,y) = a
    if (x < 0)
      HFalse +| s"$x < 0"
    else if (y < 0)
      HFalse +| s"$y < 0"
    else if (x >= p)
      HFalse +| s"$x >= $p"
    else if (y >= p)
      HFalse +| s"$y >= $p"
    else
      HTrue +| s"0 <= $x < $p and 0 <= $y < $p"
  } ++ Map("a" -> a)).annotate("member")

}

object testGaussianInt {
  def main(argv: Array[String]): Unit = {

    for {p <- (2 to 17).view
         m = new GaussianIntModP(p)
         f = Magma.isField(m.gen, m.member,
                           m.add, m.mult,
                           (a:(Int,Int)) => Some(m.subtract(m.zero, a)),
                           m.mult_inv,
                           m.one, m.zero
                           ).conjFalse(Map("reason" -> s"$m not a field")) &&
           HTrue +| s"$m is a field"
         } println(s"$p: $f")
  }
}