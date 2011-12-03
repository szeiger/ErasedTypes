package hltest

// Church Numerals

sealed trait Nat {
  type Self <: Nat
  type Apply[F[_], _]
  type + [_ <: Nat] <: Nat
  type * [_ <: Nat] <: Nat
  type Flip_^ [_ <: Nat] <: Nat
  type ^ [T <: Nat] = T # Flip_^[Self]
  type ++ = Succ[Self]
  def self: Self
  def ++ : ++ = new Succ[Self](value+1)
  def + [T <: Nat](n: T): +[T] = Nat.unsafe[+[T]](value + n.value)
  def * [T <: Nat](n: T): *[T] = Nat.unsafe[*[T]](value * n.value)
  def ^ [T <: Nat](n: T): ^[T] = Nat.unsafe[^[T]](Math.pow(value, n.value).asInstanceOf[Int])
  def value: Int
  override def toString = value.toString
  type _0 = *[Nat._10]
  type _1 = _0 # ++
  type _2 = _1 # ++
  type _3 = _2 # ++
  type _4 = _3 # ++
  type _5 = _4 # ++
  type _6 = _5 # ++
  type _7 = _6 # ++
  type _8 = _7 # ++
  type _9 = _8 # ++
  def _0 = this * Nat._10
  def _1 = _0 + Nat._1
  def _2 = _0 + Nat._2
  def _3 = _0 + Nat._3
  def _4 = _0 + Nat._4
  def _5 = _0 + Nat._5
  def _6 = _0 + Nat._6
  def _7 = _0 + Nat._7
  def _8 = _0 + Nat._8
  def _9 = _0 + Nat._9
}

object Nat {
  def unsafe[T <: Nat](value: Int) =
    (if(value == 0) Zero else new Succ(value)).asInstanceOf[T]
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
  type _10 = _9 # ++
  val _0: _0 = Zero
  val _1: _1 = _0 ++
  val _2: _2 = _1 ++
  val _3: _3 = _2 ++
  val _4: _4 = _3 ++
  val _5: _5 = _4 ++
  val _6: _6 = _5 ++
  val _7: _7 = _6 ++
  val _8: _8 = _7 ++
  val _9: _9 = _8 ++
  val _10: _10 = _9 ++
}

final object Zero extends Nat {
  type Self = Zero.type
  type Apply[F[_], X] = X
  type + [X <: Nat] = X
  type * [_ <: Nat] = Self
  type Flip_^ [_ <: Nat] = ++
  def self = this
  def value = 0
}

final class Succ[N <: Nat] private[hltest] (val value: Int) extends Nat {
  type Self = Succ[N]
  type -- = N
  type Apply[F[_], X] = F[N#Apply[F, X]]
  type + [X <: Nat] = Succ[N # + [X]]
  type * [X <: Nat] = (-- # * [X]) # + [X]
  type Flip_^ [X <: Nat] = (-- # Flip_^ [X]) # * [X]
  def self = this
  def -- : -- = (if(value == 1) Zero else new Succ(value-1)).asInstanceOf[--]
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
    println( (_2 ^ _3): _8 )
    println( (_3 ^ _2): _9 )
    println( _1._0: _10 )
    println( _1._6: (_8 # * [_2]) )
    
    val x: List[List[List[String]]] = (null: _3#Apply[List, String])
  }

  val l1 = 42 :: "foo" :: 1.0 :: "bar" :: HNil
  val l1a = l1.head
  val l1b = l1.tail.head
  val l1c = l1.tail.tail.head
  val l1d = l1.tail.tail.tail.head
  
  ()

}
