package com.novocode.erased

// KList

sealed trait KList { kl =>
  type Self <: KList
  type Cons[+_]
  type Head
  type Tail <: KList
  type Fold[U, F[_ <: KList, _ <: U] <: U, Z <: U] <: U
  
  type Drop[N <: Nat] = N#Fold[KList, ({ type L[X <: KList] = X#Tail })#L, Self]
  type Apply[N <: Nat] = ({ type L[X <: KList] = X#Head })#L[Drop[N]] // Drop[N]#Head
  type Length = Fold[Nat, ({ type L[X <: KList, Z <: Nat] = Succ[Z] })#L, Nat._0]
  type |: [E] = KCons[Cons, E, Self]
  type |:: [L <: KList] = L#Fold[KList, ({ type L[X <: KList, Z <: KList] = Z # |: [X#Head] })#L, Self]
  type AsIdentity = Fold[KList, ({ type L[X <: KList, Z <: KList] = Z # |: [X#Cons[X#Head]] })#L, KNil[KList.Identity]]
  type Map[To[+_]] = Fold[KList, ({ type L[X <: KList, Z <: KList] = Z # |: [X#Head] })#L, KNil[To]]

  def head: Cons[Head]
  def tail: Tail
  def self: Self
  def fold[U, F[_ <: KList, _ <: U] <: U, Z <: U](f: TypedFunction2[KList, U, U, F], z: Z): Fold[U, F, Z]

  final def map[To[+_]](f: NaturalTransformation1[Cons, To]): Self#Map[To] = self.fold[KList, ({ type L[X <: KList, Z <: KList] = Z # |: [X#Head] })#L, KNil[To]](
      new TypedFunction2[KList, KList, KList, ({ type L[X <: KList, Z <: KList] = Z # |: [X#Head] })#L] {
        def apply[X <: KList, Z <: KList](x: X, z: Z): (Z # |: [X#Head]) = {
          val tf = f.asInstanceOf[NaturalTransformation1[x.Cons, z.Cons]]
          val newEU = tf.apply[X#Head](x.head)
          val newE: z.Cons[X#Head] = newEU
          newE |: z
        }
      }, KNil[To])

  final def asIdentity = this.asInstanceOf[Self#AsIdentity]
  final def length: Length = {
    var i = 0
    foreach { _ => i += 1 }
    Nat.unsafe[Length](i)
  }
  final def |: [@specialized E](elem: Cons[E]): |: [E] = new KCons[Cons, E, Self](elem, this.asInstanceOf[Self])
  /*final def |:: [L <: KList { type Cons[X] = kl.Cons[X] }](l: L): |:: [L] = l.fold[KList { type Cons[X] = kl.Cons[X] }, ({ type L[X <: KList { type Cons[X] = kl.Cons[X] }, Z <: KList { type Cons[X] = kl.Cons[X] }] = Z # |: [X#Head] })#L, Self](
      new TypedFunction2[KList { type Cons[X] = kl.Cons[X] }, KList { type Cons[X] = kl.Cons[X] }, KList { type Cons[X] = kl.Cons[X] }, ({ type L[X <: KList { type Cons[X] = kl.Cons[X] }, Z <: KList { type Cons[X] = kl.Cons[X] }] = Z # |: [X#Head] })#L] {
        def apply[P1 <: KList, P2 <: KList { type Cons[X] = kl.Cons[X] }](p1: P1, p2: P2) = p1.head |: p2
      }, self)*/

  final def drop [N <: Nat](n: N): Drop[N] = {
    var t: KList = this
    var i = n.value
    while(i > 0) {
      i -= 1
      t = t.asInstanceOf[KCons[KList.AnyCons, _,_ <: KList]].tail 
    }
    t
  }.asInstanceOf[Drop[N]]
  final def apply [N <: Nat](n: N) = drop(n).head.asInstanceOf[Cons[Apply[N]]]
  final def foreach(f: Any => Unit) {
    var n: KList = this
    while(n.isInstanceOf[KCons[Cons,_,_]]) {
      val c = n.asInstanceOf[KCons[Cons, _, _ <: KList]]
      f(c.head)
      n = c.tail
    }
  }
  override final def toString = {
    val b = new StringBuffer
    foreach { v =>
      b append v append " |: "
    }
    b append "KNil"
    b toString
  }
}

final object KList {
  type AnyCons[+X] = A[X] forSome { type A[+_] }
  type |: [F[+_], H, T <: KList { type Cons[X] = F[X] }] = KCons[F, H, T]
  type ||: [F[+_], H, N] = KCons[F, H, KCons[F, N, KNil[F]]]
  type Identity[+X] = X
  type KHList = KList { type Cons[X] = X }
}

final class KCons[F[+_], @specialized H, T <: KList](val head: F[H], val tail: T) extends KList {
  type Cons[+X] = F[X]
  type Self = KCons[Cons, H, T]
  type Head = H
  type Tail = T
  type Fold[U, F[_ <: KList, _ <: U] <: U, Z <: U] = F[Self, T#Fold[U, F, Z]]

  def self = this
  def fold[U, F[_ <: KList, _ <: U] <: U, Z <: U](f: TypedFunction2[KList, U, U, F], z: Z) =
    f.apply[Self, T#Fold[U, F, Z]](self, tail.fold[U, F, Z](f, z))
}

final class KNil[F[+_]] extends KList {
  type Cons[+X] = F[X]
  type Self = KNil[Cons]
  type Head = Nothing
  type Tail = Nothing
  type Fold[U, F[_ <: KList, _ <: U] <: U, Z <: U] = Z

  def self = this
  def head = sys.error("KNil.head")
  def tail = sys.error("KNil.tail")
  def fold[U, F[_ <: KList, _ <: U] <: U, Z <: U](f: TypedFunction2[KList, U, U, F], z: Z) = z
}

object KNil {
  def apply[F[+_]] = new KNil[F]
  def unapply(k: KNil[KList.AnyCons]) = Some(k)
}
