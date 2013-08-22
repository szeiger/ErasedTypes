package com.novocode.erased

import scala.language.experimental.macros
import scala.reflect.macros.Context

/** Church Numerals
 * 
 * Value-level computations at run-time are done directly without using the
 * Church encoding of the type level. The erased version is very similar to
 * java.lang.Integer.
 */
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
  def ^ [T <: Nat](n: T): ^[T] = Nat.unsafe[^[T]](scala.math.pow(value, n.value).asInstanceOf[Int])
  def value: Int
  def self: Self
  override def toString = value.toString
  type _0 = Self # * [Nat._10]
  type _1 = _0 # + [Nat._1]
  type _2 = _0 # + [Nat._2]
  type _3 = _0 # + [Nat._3]
  type _4 = _0 # + [Nat._4]
  type _5 = _0 # + [Nat._5]
  type _6 = _0 # + [Nat._6]
  type _7 = _0 # + [Nat._7]
  type _8 = _0 # + [Nat._8]
  type _9 = _0 # + [Nat._9]
  def _0 = (self * Nat._10): _0
  def _1 = (_0 + Nat._1): _1
  def _2 = (_0 + Nat._2): _2
  def _3 = (_0 + Nat._3): _3
  def _4 = (_0 + Nat._4): _4
  def _5 = (_0 + Nat._5): _5
  def _6 = (_0 + Nat._6): _6
  def _7 = (_0 + Nat._7): _7
  def _8 = (_0 + Nat._8): _8
  def _9 = (_0 + Nat._9): _9

  override def equals(o: Any) = o match {
    case n: Nat => value == n.value
    case _ => false
  }
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

  def apply(i: Int): Nat = macro Nat.applyImpl
  def applyImpl(ctx: Context)(i: ctx.Expr[Int]): ctx.Expr[Nat] = {
    import ctx.universe._
    println(showRaw(i.tree))
    val _Nat = typeOf[Nat.type].typeSymbol.companionSymbol
    val _Succ = typeOf[Succ[_]].typeSymbol
    val _Zero = typeOf[Zero.type].typeSymbol.companionSymbol

    i.tree match {
      case Literal(Constant(v: Int)) =>
        val tt = (1 to v).foldLeft[Tree](SingletonTypeTree(Ident(_Zero))) { case (z, _) =>
          AppliedTypeTree(Ident(_Succ), List(z))
        }
        ctx.Expr(
          Apply(
            TypeApply(
              Select(Ident(_Nat), newTermName("unsafe")),
              List(tt)),
            List(Literal(Constant(v)))))
      case _ => reify(Nat.unsafe[Nat](i.splice))
    }
  }
}

final object Zero extends Nat {
  type Self = Zero.type
  type Fold[U, F[_ <: U] <: U, Z <: U] = Z
  type + [X <: Nat] = X
  type * [_ <: Nat] = Nat._0
  type Flip_^ [_ <: Nat] = Nat._1
  def value = 0
  def self = this
}

final class Succ[N <: Nat] private[erased] (val value: Int) extends Nat {
  //def this(p: N) = this(p.value+1)
  type Self = Succ[N]
  type -- = N
  type Fold[U, F[_ <: U] <: U, Z <: U] = F[N#Fold[U, F, Z]]
  type + [X <: Nat] = Succ[N # + [X]]
  type * [X <: Nat] = (N # * [X]) # + [X]
  type Flip_^ [X <: Nat] = (N # Flip_^ [X]) # * [X]
  def -- : -- = Nat.unsafe[--](value-1)
  def self = this
}
