package com.novocode.erased

// KList

sealed trait KList { kl =>
  import KList._

  type LikeThis = KList { type Cons[+X] = kl.Cons[X] }
  type Self <: LikeThis
  type Cons[+_]
  type Head
  type Tail <: LikeThis
  type Fold[U, F[_ <: LikeThis, _ <: U] <: U, Z <: U] <: U
  
  type Drop[N <: Nat] = N#Fold[KList, ({ type L[X <: KList] = X#Tail })#L, Self]
  type Apply[N <: Nat] = ({ type L[X <: KList] = X#Head })#L[Drop[N]] // Drop[N]#Head
  type Length = Fold[Nat, ({ type L[X <: KList, Z <: Nat] = Succ[Z] })#L, Nat._0]
  type |: [E] = KCons[Self#Cons, E, Self]
  type |:: [O <: LikeThis] = O#Fold[O#LikeThis, ({ type L[X <: O#LikeThis, Z <: O#LikeThis] = Z # |: [X#Head] })#L, Self]
  type AsIdentity = Fold[KList, ({ type L[X <: KList, Z <: KList] = Z # |: [X#Cons[X#Head]] })#L, KNil[Identity]]
  //type Map[To[+_]] = Fold[KListOf[To], ({ type L[X <: LikeThis, Z <: KListOf[To]] = Z # |: [X#Head] })#L, KNil[To]]
  type Map[To[+_]] = Fold[KListOf[To], ({ type L[X <: LikeThis, Z <: KListOf[To]] = KCons[Z#Self#Cons, X#Head, Z#Self] })#L, KNil[To]]

  def head: Cons[Head]
  def tail: Tail
  def self: Self
  def fold[U, F[FN <: LikeThis, FZ <: U] <: U, Z <: U](f: TypedFunction2[LikeThis, U, U, F], z: Z): Fold[U, F, Z]

  final def map[To[+_]](f: (Cons ~> To)): Self#Map[To] = self.fold[KListOf[To], ({ type L[X <: LikeThis, Z <: KListOf[To]] = KCons[Z#Self#Cons, X#Head, Z#Self] })#L, KNil[To]](
      new TypedFunction2[LikeThis, KListOf[To], KListOf[To], ({ type L[X <: LikeThis, Z <: KListOf[To]] = KCons[Z#Self#Cons, X#Head, Z#Self] })#L] {
        def apply[X <: LikeThis, Z <: KListOf[To]](x: X, z: Z) = {
          val app = f.apply[X#Head](x.head)
          new KCons[Z#Self#Cons, X#Head, Z#Self](app, z.self)
        }
      }, KNil[To])

  final def asIdentity = this.asInstanceOf[Self#AsIdentity]
  final def length: Length = {
    var i = 0
    foreach { _ => i += 1 }
    Nat.unsafe[Length](i)
  }
  final def |: [E](elem: Self#Cons[E]): |: [E] = new KCons[Self#Cons, E, Self](elem, self)
  final def |:: [O <: LikeThis](o: O): |:: [O] = o.fold[O#LikeThis, ({ type L[X <: O#LikeThis, Z <: O#LikeThis] = Z # |: [X#Head] })#L, Self](
      new TypedFunction2[O#LikeThis, O#LikeThis, O#LikeThis, ({ type L[X <: O#LikeThis, Z <: O#LikeThis] = Z # |: [X#Head] })#L] {
        def apply[P1 <: O#LikeThis, P2 <: O#LikeThis](p1: P1, p2: P2) = p1.head |: p2
      }, self)

  final def drop [N <: Nat](n: N): Drop[N] = {
    var t: KList = this
    var i = n.value
    while(i > 0) {
      i -= 1
      t = t.asInstanceOf[KCons[NaturalTransformation.__, _,_ <: KList]].tail 
    }
    t
  }.asInstanceOf[Drop[N]]
  final def apply [N <: Nat](n: N) = drop(n).head.asInstanceOf[Cons[Apply[N]]]
  final def foreach(f: Any => Unit) {
    var n: KList = this
    while(n.isInstanceOf[KCons[NaturalTransformation.__, _,_]]) {
      val c = n.asInstanceOf[KCons[NaturalTransformation.__, _, _ <: KList]]
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
  type KListOf[F[+_]] = KList { type Cons[+X] = F[X] }
  //type |: [H, T <: KList] = KCons[T#Cons, H, T#Self]
  //type |:: [K1 <: KList, K2 <: K1#LikeThis] = K2 # |:: [K1]
  type Identity[+X] = X
  type KHList = KList { type Cons[X] = X }
}

final class KCons[F[+_], H, T <: KList { type Cons[X] = F[X] }](val head: F[H], val tail: T) extends KList { kl =>
  type Cons[+X] = F[X]
  type Self = KCons[F, H, T]
  type Head = H
  type Tail = T
  type Fold[U, F[_ <: LikeThis, _ <: U] <: U, Z <: U] = F[Self, T#Fold[U, F, Z]]

  def self = this
  def fold[U, F[_ <: LikeThis, _ <: U] <: U, Z <: U](f: TypedFunction2[LikeThis, U, U, F], z: Z) =
    f.apply[Self, T#Fold[U, F, Z]](self, tail.fold[U, F, Z](f, z))
}

final class KNil[F[+_]] extends KList {
  type Cons[+X] = F[X]
  type Self = KNil[Cons]
  type Head = Nothing
  type Tail = Nothing
  type Fold[U, F[_ <: LikeThis, _ <: U] <: U, Z <: U] = Z

  def self = this
  def head = sys.error("KNil.head")
  def tail = sys.error("KNil.tail")
  def fold[U, F[_ <: LikeThis, _ <: U] <: U, Z <: U](f: TypedFunction2[LikeThis, U, U, F], z: Z) = z
}

object KNil {
  def apply[F[+_]] = new KNil[F]
  def unapply(k: KNil[NaturalTransformation.__]) = Some(k)
}
