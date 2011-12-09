package hltest
import scala.annotation.tailrec

// Church Numerals

sealed trait Nat {
  type Self <: Nat
  type Fold[U, F[_ <: U] <: U, Z <: U] <: U
  type + [_ <: Nat] <: Nat
  type * [_ <: Nat] <: Nat
  type Flip_^ [_ <: Nat] <: Nat
  type ^ [T <: Nat] = T # Flip_^[Self]
  type ++ = Succ[Self]
  def ++ = Nat.unsafe[++](value+1)
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
    (if(value < cached.length) cached(value) else new Succ(value)).asInstanceOf[T]
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
  val _1 = new Succ(1).asInstanceOf[_1]
  val _2 = new Succ(2).asInstanceOf[_2]
  val _3 = new Succ(3).asInstanceOf[_3]
  val _4 = new Succ(4).asInstanceOf[_4]
  val _5 = new Succ(5).asInstanceOf[_5]
  val _6 = new Succ(6).asInstanceOf[_6]
  val _7 = new Succ(7).asInstanceOf[_7]
  val _8 = new Succ(8).asInstanceOf[_8]
  val _9 = new Succ(9).asInstanceOf[_9]
  val _10 = new Succ(10).asInstanceOf[_10]
  private[this] val cached = Array(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10)
}

final object Zero extends Nat {
  type Self = Zero.type
  type Fold[U, F[_ <: U] <: U, Z <: U] = Z
  type + [X <: Nat] = X
  type * [_ <: Nat] = Nat._0
  type Flip_^ [_ <: Nat] = Nat._1
  def value = 0
}

final class Succ[N <: Nat] private[hltest] (val value: Int) extends Nat {
  type Self = Succ[N]
  type -- = N
  type Fold[U, F[_ <: U] <: U, Z <: U] = F[N#Fold[U, F, Z]]
  type + [X <: Nat] = Succ[N # + [X]]
  type * [X <: Nat] = (N # * [X]) # + [X]
  type Flip_^ [X <: Nat] = (N # Flip_^ [X]) # * [X]
  def -- : -- = Nat.unsafe[--](value-1)
}

// HList

trait HList {
  type Self <: HList
  type Head
  type Tail <: HList
  type Length <: Nat
  type Drop[N <: Nat] = N#Fold[HList, ({ type L[X <: HList] = X#Tail })#L, Self]
  type Apply[N <: Nat] = ({ type L[X <: HList] = X#Head })#L[Drop[N]] // Drop[N]#Head
  def head: Head
  def length: Length
  def :: [E](elem: E) = new HCons[E, Self](elem, this.asInstanceOf[Self])
  final def drop [N <: Nat](n: N): Drop[N] = {
    var t: HList = this
    var i = n.value
    while(i > 0) {
      i -= 1
      t = t.asInstanceOf[HCons[_,_ <: HList]].tail 
    }
    t
  }.asInstanceOf[Drop[N]]
  final def apply [N <: Nat](n: N) = drop(n).head.asInstanceOf[Apply[N]]
  def foreach(f: Any => Unit) {
    var n: HList = this
    while(n.isInstanceOf[HCons[_,_]]) {
      val c = n.asInstanceOf[HCons[_, _ <: HList]]
      f(c.head)
      n = c.tail
    }
  }
  override def toString = {
    val b = new StringBuffer
    foreach { v =>
      b append v append " :: "
    }
    b append "HNil"
    b toString
  }
}

class HCons[H, T <: HList](val head: H, val tail: T) extends HList {
  type Head = H
  type Tail = T
  type Self = HCons[H, T]
  type Length = T#Length # ++
  def length = tail.length++
  def self = this
}

object HNil extends HList {
  type Self = HNil.type
  type Head = Nothing
  type Tail = Nothing
  type Length = Zero.type
  def self = HNil
  def head = error("HNil.head")
  def length = Nat._0
}



object HLTest extends App {

  // Test the Church Numerals
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
    
    val x: List[List[List[String]]] = (null: _3#Fold[Any, List, String])
  }

  // Test the HList
  {
    val l1 = 42 :: "foo" :: Some(1.0) :: "bar" :: HNil
    val l1a = l1.head
    val l1b = l1.tail.head
    val l1c = l1.tail.tail.head
    val l1d = l1.tail.tail.tail.head
    
    println(l1)
    val l2 = l1.drop(Nat._3)
	  println(l2)
    val e0: Int = l1(Nat._0)
    val e2a: Option[Double] = l1.apply(Nat._2)
    val e2b: Option[Double] = l1.drop(Nat._2).head

    val x1 = null : l1.type#Tail#Tail#Tail#Head
    val x2 = null : Nat._3#Fold[HList, ({ type L[X <: HList] = X#Tail })#L, l1.type#Self]#Head
    val x3: Option[Double] = null : l1.type#Drop[Nat._2]#Head

    implicitly[l1.Length <:< Nat._4]
    implicitly[l2.Length <:< Nat._1]
  }

}
