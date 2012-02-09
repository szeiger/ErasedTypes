package com.novocode.erased

/**
 * A natural transformation.
 */
trait NaturalTransformation

object NaturalTransformation {
  val optionToSeq = new (Option ~> Seq) {
    def apply[T](v: Option[T]) = v match {
      case Some(x) => Seq(x)
      case None => Seq.empty[T]
    }
  }
  type Const[C] = { type L[_] = C }
  type __ [+X] = A[X] forSome { type A[+_] }
  val seqToLength = new (Seq ~> Const[Int]#L) {
    def apply[T](s: Seq[T]) = s.length
  }
}

trait ~> [-P1[_], +R[_]] extends NaturalTransformation {
  def apply[T](v: P1[T]): R[T]
}
