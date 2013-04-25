package com.novocode.erased

import scala.annotation.unchecked.{uncheckedVariance => uv}

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
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedFunction2[HList, U, U, F], z: Z): Fold[U, F, Z]

  final def length: Length = {
    var i = 0
    foreach { _ => i += 1 }
    Nat.unsafe[Length](i)
  }
  final def |: [@specialized E](elem: E): |: [E] = new HCons[E, Self](elem, this.asInstanceOf[Self])
  final def |:: [L <: HList](l: L): |:: [L] = l.fold[HList, ({ type L[X <: HList, Z <: HList] = Z # |: [X#Head] })#L, Self](
      new TypedFunction2[HList, HList, HList, ({ type L[X <: HList, Z <: HList] = Z # |: [X#Head] })#L] {
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
  type |: [+H, +T <: HList] = HCons[H, T]
  type ||: [+H, +N] = HCons[H, HCons[N, HNil]]
}

final class HCons[@specialized +H, +T <: HList](val head: H, val tail: T) extends HList {
  type Self = HCons[H @uv, T @uv]
  type Head = H @uv
  type Tail = T @uv
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] = F[Self @uv, (T @uv)#Fold[U, F, Z]]

  def self = this
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedFunction2[HList, U, U, F], z: Z): Fold[U, F, Z] @uv =
    f.apply[Self, T#Fold[U, F, Z]](self, tail.fold[U, F, Z](f, z))
}

final object HNil extends HList {
  type Self = HNil.type
  type Head = Nothing
  type Tail = Nothing
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] = Z

  def self = HNil
  def head = sys.error("HNil.head")
  def tail = sys.error("HNil.tail")
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedFunction2[HList, U, U, F], z: Z) = z
}
