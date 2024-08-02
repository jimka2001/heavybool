package heavybool
import cats.Foldable
import cats.syntax.all._
import heavybool.examples.DynMagma

abstract class Magma[T,C[_]:Foldable] {
  import HeavyBool._

  def gen(): C[T]

  def op(a: T, b: T): T

  def member(a: T): HeavyBool

  def equiv(a: T, b: T): HeavyBool = {
    if (a == b)
      HTrue
    else
      HFalse
  }.tag("equivalent") ++ Map("a" -> a, "b" -> b)

  def isClosed(): HeavyBool = {
    forallM("a", gen()){ (a:T) =>
      forallM("b", gen()) { (b: T) =>
        member(op(a, b)) ++ Map("op(a,b)" -> op(a, b))
      }}}.tag("closed")

  def isAssociative(): HeavyBool = {
    forallM("a", gen()) { (a:T) =>
      forallM("b", gen()) { (b:T) =>
        forallM("c", gen()) { (c:T) =>
          equiv(op(op(a, b), c),
                op(a, op(b, c)))
        }}}}.tag("associative")

  def isCommutative(): HeavyBool = {
    forallM("a", gen()) { (a:T) =>
      forallM("b", gen()) { (b: T) =>
        equiv(op(a, b), op(b, a))
      }}}.tag("commutative")

  def isIdentity(z: T): HeavyBool = {
    forallM("a", gen()) { (a:T) =>
      equiv(op(a, z), a) && equiv(op(z, a), a)
    }}.tag("identity") ++ Map("z" -> z)

  def findIdentity(): Option[T] = {
    gen().find(z => isIdentity(z) match {
      case HeavyTrue(_) => true
      case _ => false
    })
  }

  def findInverse(z: T, a: T): Option[T] = {
    gen().find(b => (equiv(z, op(a, b)) && equiv(z, op(b, a))).toBoolean)
  }

  def findInverse(a: T): Option[T] = {
    findIdentity() match {
      case None => None
      case Some(z) => findInverse(z, a)
    }
  }

  def isInverter(z: T, invert: T => Option[T]): HeavyBool = {
    forallM("a", gen()) { (a:T) =>
      invert(a) match {
        case None => HFalse +| s"because $a has no inverse"
        case Some(b) =>
          member(b) &&
            equiv(z, op(a, b)) &&
            equiv(z, op(b, a))
      }}}.tag("find inverter") ++ Map("z" -> z)

  def isMagma(): HeavyBool = {
    isClosed()
  }.tag("magma")

  def isSemiGroup(): HeavyBool = {
    (isMagma() && isAssociative())
  }.tag("semigroup")

  def isMonoid(z: T): HeavyBool = {
    (isSemiGroup() && isIdentity(z))
  }.tag("monoid")

  def isGroup(z: T, invert: T => Option[T]): HeavyBool = {
    (isMonoid(z) && isInverter(z, invert))
  }.tag("group")

  def isGroup(): HeavyBool = {
    (findIdentity() match {
      case None => HFalse
      case Some(e) => isGroup(e, findInverse)
    }).tag("group")
  }
}

object Magma {

  import HeavyBool._

  // generate a lazy list from 0 to n inclusive
  def genLazyFinite(n: Int): LazyList[Int] = {
    LazyList.from(0 to n)
  }

  def genListFinite(n: Int): List[Int] = {
    List.from(0 to n)
  }

  def cayleyTable[T](elements: Seq[T], op: (T, T) => T): String = {
    val header: String = "*|" ++ elements.map(x => s"$x").mkString(" ")
    val divider: String = "-+" ++ elements.map(x => "-").mkString("-")
    "\n" ++ header ++ "\n" ++ divider ++ "\n" ++ elements
      .map(x => elements.map(y => s"${op(x, y)}")
        .mkString(s"$x|", " ", ""))
      .mkString("\n")
  }


  def randomCayleyTable(n:Int):(Int,Int)=>Int = {
    import scala.util.Random
    // generate a function representing one possible addition table of a
    // magma of n elements, numbers 0 to n-1 with the restriction
    // that 0+z= 0 and z+0 = z
    val tbl = (for{row <- 1 to n-1
                   col <- 1 to n-1 }
    yield (row,col) -> Random.between(0,n)).toMap

    def add(a: Int, b: Int): Int = {
        if (a == 0)
          b
        else if (b == 0)
          a
        else tbl((a,b))
    }

    add
  }

  def randomCayleyTables(n:Int):LazyList[(Int,Int)=>Int] = {
    randomCayleyTable(n) #:: randomCayleyTables(n)
  }

  def allUnitalCayleyTables(n: Int): LazyList[(Int, Int) => Int] = {
    import scala.math.pow
    // generate a lazy sequence of length n^n * (n-1)^2 of functions
    // each representing one possible addition table of a
    // magma of n elements, numbers 0 to n-1 with the restriction
    // that 0+z= 0 and z+0 = z
    def recur(i: Int): LazyList[(Int, Int) => Int] = {
      def add(a: Int, b: Int): Int = {
        if (a == 0)
          b
        else if (b == 0)
          a
        else {
          // interpret i as a (n-1)x(n-1) digit number in base n, i.e., composed
          // of digits 0 ... (n-1)
          // arrange these digits into an (n-1) x (n-1) matrix, and return
          // the element at row a, column b.
          // i.e. the digit in position (1 based) (a-1)*(n-1)+(b-1)
          // we know that a>0 and b>0 because of the first two clauses
          // of the if/else-if.
          val pos = (a - 1) * (n - 1) + (b - 1)
          i / pow(n, pos).toInt % n
        }
      }

      if (i < 0)
        LazyList()
      else
        add _ #:: recur(i - 1)
    }
    // n ^ ((n-1)^2)
    // e.g., if n=8, we want to know how many 49 digit base 8 numbers exist?
    // i.e., 8^((8-1)*(8-1))
    // if n=2, how many 1 digit base 2 numbers = 1^2 = 2
    // if n=3, how many 4 digit base 3 numbers = 3^4 = 81
    // if n=4, how many 9 digit base 4 numbers = 4^9 = 262144
    recur(pow(n, (n - 1) * (n - 1)).toInt - 1)
  }

  def allCayleyTables(n: Int): LazyList[(Int, Int) => Int] = {
    import scala.math.pow

    def recur(i: Int): LazyList[(Int, Int) => Int] = {
      def add(a: Int, b: Int): Int = {
          // interpret i as a n x n  digit number in base n, i.e., composed
          // of digits 0 ... (n-1)
          // arrange these digits into an n x n matrix, and return
          // the element at row a, column b.
          // i.e. the digit in position (0 based) a*n+b
          val pos = a * n + b
          i / pow(n, pos).toInt % n
      }

      if (i < 0)
        LazyList()
      else
        add _ #:: recur(i - 1)
    }

    recur(pow(n, n*n).toInt - 1)
  }

  def countGroups(n:Int) = {
    val elements = genListFinite(n-1)
    var groups = 0
    var abeliangroups = 0
    var monoids = 0
    var abelianmonoids = 0
    var semigroups = 0
    var abeliansemigroups = 0
    var tries = 0
    var abelians = 0
    // find n element monoids
    for {dyn_op <- allCayleyTables(n)
         dm = DynMagma(() => elements,
                       op1 = dyn_op,
                       member1 = (a: Int) => elements.contains(a)
                       )
         ab = dm.isCommutative()
         } {
      tries += 1
      if (ab.toBoolean)
        abelians += 1
      (dm.isGroup(0, (x:Int) => dm.findInverse(x)) match {
        case tf: HeavyFalse => tf // println(str)
        case tf: HeavyTrue =>
          groups += 1
          monoids += 1
          semigroups += 1
          if (ab.toBoolean) {
            abeliangroups += 1
            abelianmonoids += 1
            abeliansemigroups += 1
          }
          print("found"
                  + (if (ab.toBoolean) " Abelian" else "")
                  + s" group ${cayleyTable(elements, dyn_op)} ")
          tf ++ Map("reason" -> (s"found a group " + cayleyTable(elements, dyn_op)))
      }) ||
        (dm.isMonoid(0) match {
          case tf: HeavyFalse => tf //println(str)
          case tf: HeavyTrue =>
            monoids += 1
            semigroups += 1
            if (ab.toBoolean) {
              abelianmonoids += 1
              abeliansemigroups += 1
            }
            tf ++ Map("reason" -> ("found a monoid " + cayleyTable(elements, dyn_op)))
        }) ||
        (dm.isSemiGroup() match {
          case tf: HeavyFalse => tf //println(str)
          case tf: HeavyTrue =>
            semigroups += 1
            if (ab.toBoolean) {
              abeliansemigroups += 1
            }
            tf ++ Map("reason" -> ("found a semigroup " + cayleyTable(elements, dyn_op)))
        })
    }
    println(s"magmas: $tries   abelian=$abelians")
    println(s"semigroups: $semigroups   abelian=$abeliansemigroups")
    println(s"monoids: $monoids   abelian=$abelianmonoids")
    println(s"groups:  $groups   abelian=$abeliangroups")
  }

  def findGroupsM(n:Int) = {
    val elements = genListFinite(n-1) // [0, 1, 2] size 3
    var groups = 0
    var tries = 0

    for { add <- allUnitalCayleyTables(n)
          _ = (tries += 1)
          dm = DynMagma(() => elements,
                        op1 = add,
                        member1 = (a: Int) => elements.contains(a)
                        )
          ig = dm.isGroup(0, x => dm.findInverse(x))
          if ig.toBoolean
          }  {
      val table = cayleyTable(elements, add)
      groups += 1
      println(s"found a group " + table + s": $ig")
    }
    println(s"groups:  $groups/$tries")
  }

  def isRing[T, C[_]:Foldable](gen: () => C[T],
                member: T => HeavyBool,
                add: (T, T) => T, mult: (T, T) => T,
                invert: T => Option[T],
                one: T, zero: T): HeavyBool = {
    val ma = DynMagma[T,C](gen, add, x => member(x).toBoolean)
    val mb = DynMagma[T,C](gen, mult, x => member(x).toBoolean)
    ma.isGroup(zero, invert) &&
      ma.isCommutative() &&
      mb.isMonoid(one) &&
      forallM("a", gen()) { a =>
        forallM("b", gen()) { b =>
          forallM("c", gen()) { c =>
            // left distribute
            ma.equiv(mult(a, add(b, c)),
                     add(mult(a, b), mult(a, c)))
              .conjFalse(Map("reason" -> s"$a does not left-distribute across ($b+$c)")) &&
            // right distribute
              ma.equiv(mult(add(b,c),a),
                       add(mult(b,a),mult(c,a)))
                .conjFalse(Map("reason" -> s"$a does not right-distribute across ($b+$c)"))
  }}}}

  def isField[T,C[_]:Foldable](gen: () => C[T],
                 member: T => HeavyBool,
                 add: (T, T) => T, mult: (T, T) => T,
                 add_invert: T => Option[T], mult_invert: T => Option[T],
                 one: T, zero: T): HeavyBool = {
    val ma = DynMagma[T,C](gen, add, x => member(x).toBoolean)
    lazy val mb = DynMagma[T,C](gen, mult, x => member(x).toBoolean)
    lazy val mz = DynMagma[T,List](non_zero_gen, mult, x => member(x).toBoolean)
    def non_zero_gen():List[T] = {
      gen().filter_(_ != zero)
    }
    !ma.equiv(one,zero) &&
      mb.isCommutative().conjFalse(Map("reason" -> "not commutative")) &&
      isRing(gen, member,
             add, mult, add_invert,
             one, zero).conjFalse(Map("reason" -> s"not a ring")) &&
      mz.isInverter(one, mult_invert)
        .conjFalse(Map("reason" -> "invalid inversion"))
  }

  def main(argv: Array[String]): Unit = {

  }
}

