package hltest

// Church Numerals

sealed trait Nat {
  type Self <: Nat
  type Apply[_]
  type + [_ <: Nat] <: Nat
  type * [_ <: Nat] <: Nat
  type ++ = Succ[Self]
  def self: Self
  def ++ : ++ = new Succ[Self](value+1)
  def + [T <: Nat](n: T): +[T]
  def value: Int
  override def toString = value.toString
}

object Nat {
  def apply(value: Int): Nat =
    if(value == null) Zero else new Succ(value)
  type _0 = Zero.type
  type _1 = _0 # ++
  type _2 = _1 # ++
  type _3 = _2 # ++
  type _4 = _3 # ++
  type _5 = _4 # ++
  type _6 = _5 # ++
  type _7 = _6 # ++
  type _8 = _7 # ++
  type _9 = _8 # ++
  val _0 = Zero
  val _1 = _0 ++
  val _2 = _1 ++
  val _3 = _2 ++
  val _4 = _3 ++
  val _5 = _4 ++
  val _6 = _5 ++
  val _7 = _6 ++
  val _8 = _7 ++
  val _9 = _8 ++
}

final object Zero extends Nat {
  type Self = Zero.type
  type Apply[X] = X
  type + [X <: Nat] = X
  type * [_ <: Nat] = Self
  def self = this
  def value = 0
  def + [T <: Nat](n: T): +[T] = n
}

final class Succ[N <: Nat] private[hltest] (val value: Int) extends Nat {
  type Self = Succ[N]
  type -- = N
  type Apply[X] = N#Apply[X]
  type + [X <: Nat] = Succ[N # + [X]]
  type * [X <: Nat] = (-- # * [X]) # + [X]
  def self = this
  def pred: -- = (if(value == 1) Zero else new Succ(value-1)).asInstanceOf[--]
  def + [T <: Nat](n: T): +[T] = new Succ[+[T] # --](value + n.value)
}

// HList

trait HList {
  type Self <: HList
  def self: Self
  def :: [E](elem: E) = new HCons[E, Self](elem, self)
}

class HCons[H, T <: HList](val head: H, val tail: T) extends HList {
  type Head = H
  type Tail = T
  type Self = HCons[H, T]
  def self = this
}

object HNil extends HList {
  type Self = HNil.type
  def self = HNil
}





object HLTest extends App {

  {
    import Nat._;
    println( (_2 + _2): _4 )
    println( (_3 + _0): _3 )
    println( (_0 + _2): _2 )
    println( (_1 + _2): _3 )
    println( (_2 * _3): _6 )
  }

  val l1 = 42 :: "foo" :: 1.0 :: "bar" :: HNil
  val l1a = l1.head
  val l1b = l1.tail.head
  val l1c = l1.tail.tail.head
  val l1d = l1.tail.tail.tail.head
  
  ()

}
