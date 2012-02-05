package com.novocode.erased

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
