package heavybool

import adjuvant.MyFunSuite
import heavybool.HeavyBool._

class ModPSuite extends MyFunSuite {


  test("more tests") {
    for {p <- List(2, 3, 5, 7, 11)
         g = new MultiplicationModP(p)
         } {
      def inv(a: Int): Option[Int] = {
        (1 until p).find(b => (a * b) % p == 1)
      }

      assertM(g.isGroup(1, inv))
      assert(g.isGroup(1, inv).toBoolean)
    }

    for {p <- List(4, 6, 8, 9, 10)
         g = new MultiplicationModP(p)
         } {
      def inv(a: Int): Option[Int] = {
        (1 until p).find(b => (a * b) % p == 1)
      }

      assertM(!g.isGroup(1, inv))
      assert(!g.isGroup(1, inv).toBoolean)
    }
  }

  test("test mod p") {
    for {p <- 2 to 10
         add = new AdditionModP(p)
         mult = new MultiplicationModP(p)
         } {
      assertM(add.isMonoid(0))

      add.isGroup(0, (a: Int) => Some((p - a) % p)).tag(s"addition mod $p") match {
        case hb@HeavyFalse(_) => println(hb)
        case _ => println(s"$add is a group")
      }
      mult.isGroup(1, a => (1 until p).find(b => (a * b) % p == 1)).tag(s"multiplication mod $p") match {
        case hb@HeavyFalse(_) => println(hb)
        case _ => println(s"$mult is a group")
      }
    }
  }
  test("exists")  {
    def f(p: Int): HeavyBool = {
      val g = new MultiplicationModP(p)

      def inv(a: Int): Option[Int] = {

        (1 until p).find(b => (a * b) % p == 1)
      }
      g.isGroup(1, inv)
    }
    println(existsM("f", LazyList.from(3 to 3))(f))
    println(existsM("p", LazyList.from(2 to 10))(p => !f(p)))
    println(existsM("f", LazyList.from(2 to 10))(f))
  }


  test("fields") {
    for {p <- (2 to 13).view
         m = new GaussianIntModP(p)
         f = Magma.isField(m.gen, m.member,
                           m.add, m.mult,
                           (a: (Int, Int)) => Some(m.subtract(m.zero, a)),
                           m.mult_inv,
                           m.one, m.zero
                           ).conjFalse(Map("reason" -> s"$m not a field")) &&
           HTrue +| s"$m is a field"
         } if (List(3, 7, 11).contains(p)) assert(f.toBoolean) else assert(!(f.toBoolean))
  }
}
