package com.novocode.erased

import scala.language.dynamics
import scala.language.experimental.macros
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.reflect.macros.Context
import syntax._

// HArray

sealed trait HArray[+L <: HList] extends Any with Product with Dynamic {
  type -> [R]
  def -> [R](f: -> [R]): R
  @inline final def apply[N <: Nat](n: N) = productElement(n.value).asInstanceOf[L#Apply[N]]
  @inline final def unsafeApply[N <: Nat](n: Int): L#Apply[N] = productElement(n).asInstanceOf[L#Apply[N]]
  @inline final def length: L#Length = Nat._unsafe[L#Length](productArity)
  final def canEqual(that: Any) = that.isInstanceOf[HArray[_]]
  override def equals(that: Any): Boolean = that match {
    case h: HArray[_] =>
      val l = productArity
      if(l != h.productArity) return false
      var i = 0
      while(i < l) {
        if(productElement(i) != h.productElement(i)) return false
        i += 1
      }
      true
    case _ =>
      false
  }
  override final def toString = {
    val b = new StringBuilder
    b append "("
    val l = productArity
    var i = 0
    while(i < l) {
      if(i != 0) b append ","
      b append productElement(i)
      i += 1
    }
    b append ")"
    b.toString
  }

  def selectDynamic(field: String): Any = macro HArray.selectDynamicImpl
}

object HArray {
  def unsafe[L <: HList](a: Array[Any]): HArray[L] = {
    if(a.length == 2) {
      val _1 = a(0)
      val _2 = a(1)
      if(_1.isInstanceOf[Int] && _2.isInstanceOf[Int])
        HArrayII(_1.asInstanceOf[Int], _2.asInstanceOf[Int])
      else new HArray2[Any, Any](_1, _2)
    }
    else if(a.length == 3) new HArray3[Any, Any, Any](a(0), a(1), a(2))
    else new HArrayA[L](a)
  }.asInstanceOf[HArray[L]]

  def apply(vs: Any*) = macro HArray.applyImpl

  def applyImpl(ctx: Context)(vs: ctx.Expr[Any]*): ctx.Expr[HArray[A]] forSome { type A } = {
    import ctx.universe._
    if(vs.isEmpty) reify(unsafe[HNil](Array[Any]()))
    else {
      vs.head.tree match {
        case Typed(seq, Ident(tpnme.WILDCARD_STAR)) =>
          reify(unsafe[HList](Array[Any](ctx.Expr[Seq[Any]](seq).splice: _*)))
        case _ =>
          val _HArray = typeOf[HArray[_]].typeSymbol.companionSymbol
          val _HArrayA = typeOf[HArrayA[_]].typeSymbol
          val _HArray2 = typeOf[HArray2[_,_]].typeSymbol
          val _HArray3 = typeOf[HArray3[_,_,_]].typeSymbol
          val _HList = typeOf[HList].typeSymbol.companionSymbol
          val _HNil = typeOf[HNil.type].typeSymbol
          val _HCons = typeOf[HCons[_, _]].typeSymbol
          val _Array = typeOf[Array[_]].typeSymbol.companionSymbol
          val _Predef = typeOf[Predef.type].typeSymbol.companionSymbol
          if(vs.length == 2) {
            if(vs.forall(_.tree.tpe <:< definitions.IntTpe)) {
              // All Int -> new HArrayII
              val _1 = vs(0).asInstanceOf[Expr[Int]]
              val _2 = vs(1).asInstanceOf[Expr[Int]]
              reify(new HArrayII((_1.splice.toLong << 32) | (_2.splice & 0xffffffffL)))
            } else if(vs.exists(v =>
              (v.tree.tpe <:< definitions.BooleanTpe) ||
              (v.tree.tpe <:< definitions.ByteTpe) ||
              (v.tree.tpe <:< definitions.CharTpe) ||
              (v.tree.tpe <:< definitions.DoubleTpe) ||
              (v.tree.tpe <:< definitions.FloatTpe) ||
              (v.tree.tpe <:< definitions.LongTpe) ||
              (v.tree.tpe <:< definitions.ShortTpe) ||
              (v.tree.tpe <:< definitions.AnyRefTpe)
            )) {
              // Not all Int -> new HArray2
              ctx.Expr(
                Apply(
                  Select(
                    New(
                      AppliedTypeTree(
                        Ident(_HArray2),
                        vs.iterator.map(n => TypeTree(n.tree.tpe.widen)).toList
                      )
                    ),
                    nme.CONSTRUCTOR
                  ),
                  vs.iterator.map(_.tree).toList
                )
              )
            } else {
              // Unknown Intness -> Defer decision to runtime via HArray.unsafe
              ctx.Expr(
                Apply(
                  TypeApply(
                    Select(Ident(_HArray), newTermName("unsafe")),
                    List(
                      vs.foldRight[Tree](Ident(_HNil)) { case (n, z) =>
                        AppliedTypeTree(Ident(_HCons), List(TypeTree(n.tree.tpe.widen), z))
                      }
                    )
                  ),
                  List(
                    Apply(
                      TypeApply(
                        Select(Ident(_Array), newTermName("apply")),
                        List(Ident(definitions.AnyClass))
                      ),
                      vs.map(_.tree).toList
                    )
                  )
                )
              )
            }
          } else if(vs.length == 3) {
            ctx.Expr(
              Apply(
                Select(
                  New(
                    AppliedTypeTree(
                      Ident(_HArray3),
                      vs.iterator.map(n => TypeTree(n.tree.tpe.widen)).toList
                    )
                  ),
                  nme.CONSTRUCTOR
                ),
                vs.iterator.map(_.tree).toList
              )
            )
          } else {
            // new HArrayA
            ctx.Expr(
              Apply(
                Select(
                  New(
                    AppliedTypeTree(
                      Ident(_HArrayA),
                      List(
                        vs.foldRight[Tree](Ident(_HNil)) { case (n, z) =>
                          AppliedTypeTree(Ident(_HCons), List(TypeTree(n.tree.tpe.widen), z))
                        }
                      )
                    )
                  ),
                  nme.CONSTRUCTOR
                ),
                List(
                  Apply(Select(Ident(_Array), newTermName("apply")), vs.map(_.tree).toList)
                )
              )
            )
          }
      }
    }
  }

  def selectDynamicImpl(ctx: Context)(field: ctx.Expr[String]): ctx.Expr[Any] = {
    import ctx.universe._
    val idx = {
      val Expr(Literal(Constant(n: String))) = field
      (if(n.startsWith("_")) {
          val i = try Some(n.substring(1).toInt) catch { case e: NumberFormatException => None }
          i.flatMap(v => if(v < 1) None else Some(v-1))
        } else None
      ).getOrElse(ctx.abort(field.tree.pos, "value "+n+" is not a member of "+classOf[HArray[_]].getName))
    }

    val _Succ = typeOf[Succ[_]].typeSymbol
    val _Zero = typeOf[Zero.type].typeSymbol.companionSymbol
    val natT = (1 to idx).foldLeft(SingletonTypeTree(Ident(_Zero)): ctx.Tree) { case (z, _) =>
      AppliedTypeTree(Ident(_Succ), List(z))
    }

    val t = Apply(TypeApply(Select(ctx.prefix.tree, newTermName("unsafeApply")),
      List(natT)), List(Literal(Constant(idx))))
    ctx.Expr[Any](t)
  }

  // HArray.apply and HArray.unsafe ensure that this always holds
  @inline implicit def promoteHArrayII(h: HArray[Int :|: Int]) = h.asInstanceOf[HArrayII]
  @inline implicit def promoteHArray2[T1, T2](h: HArray[T1 :|: T2]) = h.asInstanceOf[HArray2[T1, T2]]
  @inline implicit def promoteHArray3[T1, T2, T3](h: HArray[T1 :: T2 :|: T3]) = h.asInstanceOf[HArray3[T1, T2, T3]]
}

final class HArrayA[L <: HList](val a: Array[Any]) extends HArray[L] {
  type -> [R] = HArrayA.FunctionA[L, R]
  def -> [R](f: -> [R]): R = f(this)
  def productArity = a.length
  def productElement(n: Int) = a(n)
}

object HArrayA {
  type FunctionA[L <: HList, R] = HArrayA[L] => R
}

final class HArray2[@specialized(Int, Long, Double, Char, Boolean, AnyRef) +T1,
    @specialized(Int, Long, Double, Char, Boolean, AnyRef) +T2](val _1: T1, val _2: T2) extends HArray[T1 :|: T2] {
  type -> [R] = (T1 @uv, T2 @uv) => R
  def -> [R](f: -> [R]): R = f(_1, _2)
  def productArity = 2
  def productElement(n: Int) = n match {
    case 0 => _1
    case 1 => _2
    case _ => throw new NoSuchElementException
  }
}

final class HArray3[@specialized(Int, Long, Double, Char, Boolean, AnyRef) +T1,
    @specialized(Int, Long, Double, Char, Boolean, AnyRef) +T2,
    @specialized(Int, Long, Double, Char, Boolean, AnyRef) +T3](val _1: T1, val _2: T2, val _3: T3) extends HArray[T1 :: T2 :|: T3] {
  type -> [R] = (T1 @uv, T2 @uv, T3 @uv) => R
  def -> [R](f: -> [R]): R = f(_1, _2, _3)
  def productArity = 3
  def productElement(n: Int) = n match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new NoSuchElementException
  }
}

final class HArrayII(val packed: Long) extends AnyVal with HArray[Int :|: Int] {
  type -> [R] = (Int, Int) => R
  def -> [R](f: -> [R]): R = f(_1, _2)
  def productArity = 2
  def productElement(n: Int) = n match {
    case 0 => _1
    case 1 => _2
    case _ => throw new NoSuchElementException
  }
  @inline def _1 = (packed >> 32).toInt
  @inline def _2 = packed.toInt
}

object HArrayII {
  @inline def apply(_1: Int, _2: Int): HArrayII =
    new HArrayII((_1.toLong << 32) | (_2 & 0xffffffffL))
}
