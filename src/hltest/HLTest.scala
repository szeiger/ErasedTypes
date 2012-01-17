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
  //def this(p: N) = this(p.value+1)
  type Self = Succ[N]
  type -- = N
  type Fold[U, F[_ <: U] <: U, Z <: U] = F[N#Fold[U, F, Z]]
  type + [X <: Nat] = Succ[N # + [X]]
  type * [X <: Nat] = (N # * [X]) # + [X]
  type Flip_^ [X <: Nat] = (N # Flip_^ [X]) # * [X]
  def -- : -- = Nat.unsafe[--](value-1)
}

// Typed Functions

trait TypedF2[T1, T2, TR, F[_ <: T1, _ <: T2] <: TR] {
  def apply[P1 <: T1, P2 <: T2](p1: P1, p2: P2): F[P1, P2]
}

// HList

sealed trait HList {
  type Self <: HList
  type Head
  type Tail <: HList
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] <: U
  
  type Drop[N <: Nat] = N#Fold[HList, ({ type L[X <: HList] = X#Tail })#L, Self]
  type Apply[N <: Nat] = ({ type L[X <: HList] = X#Head })#L[Drop[N]] // Drop[N]#Head
  type Length = Fold[Nat, ({ type L[X <: HList, Z <: Nat] = Succ[Z] })#L, Nat._0]
  type |: [E] = HCons[E, Self]
  type |:: [L <: HList] = L#Fold[HList, ({ type L[X <: HList, Z <: HList] = Z # |: [X#Head] })#L, Self] 

  def head: Head
  def tail: Tail
  def self: Self
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedF2[HList, U, U, F], z: Z): Fold[U, F, Z]

  final def length: Length = {
    var i = 0
    foreach { _ => i += 1 }
    Nat.unsafe[Length](i)
  }
  final def |: [@specialized E](elem: E): |: [E] = new HCons[E, Self](elem, this.asInstanceOf[Self])
  final def |:: [L <: HList](l: L): |:: [L] = l.fold[HList, ({ type L[X <: HList, Z <: HList] = Z # |: [X#Head] })#L, Self](
      new TypedF2[HList, HList, HList, ({ type L[X <: HList, Z <: HList] = Z # |: [X#Head] })#L] {
        def apply[P1 <: HList, P2 <: HList](p1: P1, p2: P2) = p1.head |: p2
      }, self)

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
  final def foreach(f: Any => Unit) {
    var n: HList = this
    while(n.isInstanceOf[HCons[_,_]]) {
      val c = n.asInstanceOf[HCons[_, _ <: HList]]
      f(c.head)
      n = c.tail
    }
  }
  override final def toString = {
    val b = new StringBuffer
    foreach { v =>
      b append v append " |: "
    }
    b append "HNil"
    b toString
  }
}

final object HList {
  type HNil = HNil.type
  type |: [H, T <: HList] = HCons[H, T]
  type ||: [H, N] = HCons[H, HCons[N, HNil]]
}

final class HCons[@specialized H, T <: HList](val head: H, val tail: T) extends HList {
  type Self = HCons[H, T]
  type Head = H
  type Tail = T
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] = F[Self, T#Fold[U, F, Z]]

  def self = this
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedF2[HList, U, U, F], z: Z) =
    f.apply[Self, T#Fold[U, F, Z]](self, tail.fold[U, F, Z](f, z))
}

final object HNil extends HList {
  type Self = HNil.type
  type Head = Nothing
  type Tail = Nothing
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] = Z

  def self = HNil
  def head = error("HNil.head")
  def tail = error("HNil.tail")
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedF2[HList, U, U, F], z: Z) = z
}

// KList

trait KList[M[_], L <: HList] {
  type Head = L#Head
  type Tail <: KList[M, HList#Tail]
}

// HArray

sealed trait HArray[L <: HList] extends Product {
  def apply[N <: Nat](n: N): L#Apply[N]
  def length: L#Length
  def productArity = 5
  def canEqual(that: Any) = that.isInstanceOf[HArray[_]]
  override def equals(that: Any): Boolean = that match {
    case h: HArray[_] =>
      var l = productArity
      if(l != h.productArity) return false
      var i = 0
      while(i < l) {
        if(productElement(i) != h.productElement(i)) return false
        i += 1
      }
      true
    case _ => false
  }
}

object HArray {
  import HList._
  def unsafe[L <: HList](a: Array[Any]): HArray[L] = new HArrayA[L](a)
  def apply[T1](v1: T1) = unsafe[T1 |: HNil](Array(v1))
  def apply[T1, T2](v1: T1, v2: T2) = unsafe[T1 ||: T2](Array(v1, v2))
  def apply[T1, T2, T3](v1: T1, v2: T2, v3: T3) = unsafe[T1 |: T2 ||: T3](Array(v1, v2, v3))
}

final class HArrayA[L <: HList](a: Array[Any]) extends HArray[L] {
  def apply[N <: Nat](n: N) = a(n.value).asInstanceOf[L#Apply[N]]
  def length = a.length.asInstanceOf[L#Length]
  override def toString = a.mkString("(",",",")")
  def productElement(n: Int) = a(n)
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
    val l1 = 42 |: "foo" |: Some(1.0) |: "bar" |: HNil
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
    
    println((l1.length, l2.length))

    import HList._
    val l3a = "foo" |: 42 |: HNil
    val l3b = true |: "baz" |: Some(1.0) |: HNil
    val l3 = l3a |:: l3b
    println(l3 : String |: Int |: Boolean |: String ||: Some[Double])

    val l4 = new HCons(42, new HCons(10.d, HNil))
    println(l4.getClass)
    println(l4.tail.getClass)
  }

  // Test the HArray
  {
    val v1 = HArray("foo", 42, true)
    println(v1)
    println(v1(Nat._0): String)
    println(v1(Nat._1): Int)
    println(v1(Nat._2): Boolean)
  }
}
