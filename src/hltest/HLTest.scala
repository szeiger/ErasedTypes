package hltest

// Church Numerals

sealed trait Num {
  type Self <: Num
  type Apply[_]
  type ApplyNum[_ <: Num] <: Num
  type Succ = Inc[Self]
  type Add[T <: Num] = ApplyNum[T]
  def self: Self
  def apply[T](z: T)(f: T => T): T
  def succ: Succ = Inc[Self](self)
  def + [T <: Num](n: T): Add[T]
}

object Num {
  type _0 = Zero.type
  type _1 = _0.Succ
  type _2 = _1.Succ
  type _3 = _2.Succ
  type _4 = _3.Succ
  val _0 = Zero
  val _1 = _0.succ
  val _2 = _1.succ
  val _3 = _2.succ
  val _4 = _3.succ
}

final case object Zero extends Num {
  type Self = Zero.type
  type Apply[X] = X
  type ApplyNum[X <: Num] = X
  def self = this
  def apply[T](z: T)(f: T => T) = z
  def + [T <: Num](n: T): Add[T] = n
}

final case class Inc[N <: Num](pred: N) extends Num {
  type Self = Inc[N]
  type Pred = N
  type Apply[X] = N#Apply[X]
  type ApplyNum[X <: Num] = Inc[N#ApplyNum[X]]
  def self = this
  def apply[T](z: T)(f: T => T) = f(pred.apply(z)(f))
  def + [T <: Num](n: T): Add[T] = Inc(pred + n)
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
    import Num._;
    (_2 + _2): _4
    (_3 + _0): _3
    (_0 + _2): _2
    (_1 + _2): _3
  }

  val l1 = 42 :: "foo" :: 1.0 :: "bar" :: HNil
  val l1a = l1.head
  val l1b = l1.tail.head
  val l1c = l1.tail.tail.head
  val l1d = l1.tail.tail.tail.head
  
  ()

}
