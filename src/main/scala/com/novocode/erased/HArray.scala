package com.novocode.erased

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.Context

// HArray

sealed trait HArray[L <: HList] extends Any with Product with Dynamic {
  @inline final def apply[N <: Nat](n: N) = unsafeApply[N](n.value)
  def unsafeApply[N <: Nat](n: Int): L#Apply[N]
  def length: L#Length
  def productArity = 5
  def canEqual(that: Any) = that.isInstanceOf[HArray[_]]
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
    case _ => false
  }
  def selectDynamic(field: String): Any = macro HArray.selectDynamicImpl
}

object HArray {
  import HList._
  def unsafe[L <: HList](a: Array[Any]): HArray[L] = new HArrayA[L](a)
  def apply[T1](v1: T1) = unsafe[T1 |: HNil](Array(v1))
  def apply[T1, T2](v1: T1, v2: T2) = unsafe[T1 ||: T2](Array(v1, v2))
  def apply[T1, T2, T3](v1: T1, v2: T2, v3: T3) = unsafe[T1 |: T2 ||: T3](Array(v1, v2, v3))

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
}

final class HArrayA[L <: HList](val a: Array[Any]) extends AnyVal with HArray[L] {
  def unsafeApply[N <: Nat](n: Int) = a(n).asInstanceOf[L#Apply[N]]
  def length = a.length.asInstanceOf[L#Length]
  override def toString = a.mkString("(",",",")")
  def productElement(n: Int) = a(n)
}
