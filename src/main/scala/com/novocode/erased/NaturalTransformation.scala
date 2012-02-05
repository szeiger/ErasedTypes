package com.novocode.erased

/**
 * A natural transformation.
 */
trait NaturalTransformation

trait NaturalTransformation1[-P1[_], +R[_]] extends NaturalTransformation {
  def apply[T](v: P1[T]): R[T]
}

object NaturalTransformation1 {
  val optionToSeq = new NaturalTransformation1[Option, Seq] {
    def apply[T](v: Option[T]) = v match {
      case Some(x) => Seq(x)
      case None => Seq.empty[T]
    }
  }
}
