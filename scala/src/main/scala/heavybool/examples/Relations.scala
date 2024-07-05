package heavybool.examples

import heavybool.HeavyBool
import heavybool.HeavyBool.{HTrue, existsM, forallM}

object Relations {
  def isReflexive[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    // every element relates to itself
    forallM("x", gen){(x:T) => HeavyBool(rel(x,x))}
      .tag("reflexive")
  }
  def isIrreflexive[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    // every no element relates to itself
    forallM("x", gen){(x:T) => !HeavyBool(rel(x,x))}
      .tag("irreflexive")
  }
  def isSymmetric[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    def hrel(a: T, b: T) = HeavyBool(rel(a, b))

    forallM("x", gen) { (x: T) =>
      forallM("y", gen) { (y: T) => hrel(x, y) ==> hrel(y, x) }
    }.tag("symmetric")
  }
  def isAsymmetric[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    // if a relates to b, then b does not relate to a
    def hrel(a: T, b: T) = HeavyBool(rel(a, b))

    forallM("x", gen) { (x: T) =>
      forallM("y", gen) { (y: T) => hrel(x, y) ==> !hrel(y, x) }
    }.tag("assymmetric")
  }
  def isAntisymmetric[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    // if a relates to b, and b relates to a, then a == b
    def hrel(a: T, b: T) = HeavyBool(rel(a, b))

    forallM("x", gen) { (x: T) =>
      forallM("y", gen) { (y: T) =>
        (hrel(x, y) && hrel(y, x)) ==> HeavyBool(x==y) }
    }.tag("antisymetric")
  }

  def isConnected[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    def hrel(a: T, b: T) = HeavyBool(rel(a, b))
    forallM("a", gen){ (a:T) =>
      forallM("b", gen) {(b:T) =>
        HeavyBool(a != b) ==> (hrel(a,b) || hrel(b,a))
      }
    }
  }.tag("connected")

  def isStronglyConnected[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    def hrel(a: T, b: T) = HeavyBool(rel(a, b))

    // forall a,b, either a relates to b or b relates to a
    forallM("a", gen){ (a:T) =>
      forallM("b", gen) {(b:T) =>
        hrel(a,b) || hrel(b,a)
      }
    }
  }.tag("strongly connected")

  def isTransitive[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    def hrel(a:T, b:T) = HeavyBool(rel(a,b))
    forallM("x", gen){ (x:T) =>
      forallM("y", gen){ (y:T) => {
        if (rel(x, y))
          forallM("z", gen) { (z: T) =>
            hrel(y, z) ==> hrel(x, z)
          }
        else
          HTrue
      }}}.tag("transitive")
  }

  def isEquivalence[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    isSymmetric(gen,rel) && isReflexive(gen,rel) && isTransitive(gen,rel)
  }.tag("equivalence")

  def isPartialOrder[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    isReflexive(gen,rel) && isAntisymmetric(gen,rel)  && isTransitive(gen,rel)
  }.tag("partial order")

  def isStrictPartialOrder[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    isIrreflexive(gen,rel) && isAsymmetric(gen,rel)  && isTransitive(gen,rel)
  }.tag("strict partial order")



  def main(argv:Array[String]):Unit = {
    println(existsM("a < 12", LazyList.range(1,20)){ (a:Int) => HeavyBool(a < 12)})
    println(isSymmetric(LazyList.range(1,20), (a:Int, b:Int) => a < b))
    println(isEquivalence(LazyList.range(1,20), (a:Int, b:Int) => a < b))
  }
}
