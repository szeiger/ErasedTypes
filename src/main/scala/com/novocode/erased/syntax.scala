package com.novocode.erased

/** Extra syntax for heterogenous collections. */
object syntax {
  // Use :: for types and extractors
  type :: [+H, +T <: HList] = HCons[H, T]
  val :: = HCons

  // Support the "a :|: b" shortcut syntax for "a :: b :: HNil"
  type :|: [+H, +N] = HCons[H, HCons[N, HNil.type]]
  object :|: {
    def unapply[H1, H2](l: HCons[H1, HCons[H2, HNil.type]]) = Some((l.head, l.tail.head))
  }
  implicit class HListExtensionMethods[B](val b: B) extends AnyVal {
    def :|: [A](a: A) = a :: b :: HNil
  }

  type Zero = com.novocode.erased.Zero.type
  type HNil = com.novocode.erased.HNil.type
  type True = com.novocode.erased.True.type
  type False = com.novocode.erased.False.type
}
