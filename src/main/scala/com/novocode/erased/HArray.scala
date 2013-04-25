package com.novocode.erased

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.Context
import HList._

// HArray

sealed trait HArray[+L <: HList] extends Any with Product with Dynamic {
  @inline final def apply[N <: Nat](n: N) = unsafeApply[N](n.value)
  def unsafeApply[N <: Nat](n: Int): L#Apply[N] = productElement(n).asInstanceOf[L#Apply[N]]
  def length: L#Length
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
    case _ =>
      false
  }
  override def toString = {
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
  def unsafe[L <: HList](a: Array[Any]): HArray[L] = new HArrayA[L](a)

  def apply(vs: Any*) = macro HArray.applyImpl

  def applyImpl(ctx: Context)(vs: ctx.Expr[Any]*): ctx.Expr[HArray[_]] = {
    import ctx.universe._
    if(vs.isEmpty) reify(unsafe[HNil](Array[Any]()))
    else {
      vs.head.tree match {
        case Typed(seq, Ident(tpnme.WILDCARD_STAR)) =>
          reify(unsafe[HList](Array[Any](ctx.Expr[Seq[Any]](seq).splice: _*)))
        case _ =>
          val _HArrayA = typeOf[HArrayA[_]].typeSymbol
          val _HArray2 = typeOf[HArray2[_,_]].typeSymbol
          val _HList = typeOf[HList].typeSymbol.companionSymbol
          val _HCons = typeOf[HCons[_, _]].typeSymbol
          val _Array = typeOf[Array[_]].typeSymbol.companionSymbol
          val _Predef = typeOf[Predef.type].typeSymbol.companionSymbol
          ctx.Expr(if(vs.length == 2) {
            // new HArray2
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
          } else {
            // new HArrayA
            Apply(
              Select(
                New(
                  AppliedTypeTree(
                    Ident(_HArrayA),
                    List(
                      vs.foldRight[Tree](Select(Ident(_HList), newTypeName("HNil"))) { case (n, z) =>
                        AppliedTypeTree(Ident(_HCons), List(TypeTree(n.tree.tpe.widen), z))
                      }
                    )
                  )
                ),
                nme.CONSTRUCTOR
              ),
              List(
                Apply(
                  Apply(Select(Ident(_Array), newTermName("apply")), vs.map(_.tree).toList),
                  List(Select(Ident(_Predef), newTermName("implicitly")))
                )
              )
            )
          })
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
}

final class HArrayA[L <: HList](val a: Array[Any]) extends HArray[L] {
  def length = Nat.unsafe[L#Length](a.length)
  def productArity = a.length
  def productElement(n: Int) = a(n)
}

final class HArray2[@specialized(Int, Long, Double, Char, Boolean, AnyRef) +T1,
    @specialized(Int, Long, Double, Char, Boolean, AnyRef) +T2](val _1: T1, val _2: T2) extends HArray[T1 ||: T2] {
  def length = Nat._2
  def productArity = 2
  def productElement(n: Int) = n match {
    case 0 => _1
    case 1 => _2
    case _ => throw new NoSuchElementException
  }
}
