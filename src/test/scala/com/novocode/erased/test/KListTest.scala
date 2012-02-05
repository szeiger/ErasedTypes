package com.novocode.erased.test

import org.junit.Test
import com.novocode.erased._

class KListTest {
  @Test
  def testKList {
    val l1 = 42 |: "foo" |: Some(1.0) |: "bar" |: KNil[KList.Identity]
    val l1a = l1.head
    val l1b = l1.tail.head
    val l1c = l1.tail.tail.head
    val l1d = l1.tail.tail.tail.head
    
    val o1 = Some(42) |: None |: Some(1.0) |: Some("bar") |: KNil[Option]
    implicitly[o1.type <:< (KList { type Cons[X] = Option[X] })]
    val o1a = o1.head
    val o1b = o1.tail.head
    val o1c = o1.tail.tail.head
    val o1d = o1.tail.tail.tail.head
    val o1id = o1.asIdentity
    implicitly[o1id.type <:< KList.KHList]
    implicitly[o1id.type <:< (KList { type Cons[X] = X })]
    val o1ida = o1id.head
    val o1idb = o1id.tail.head
    val o1idc = o1id.tail.tail.head
    val o1idd = o1id.tail.tail.tail.head
    val o1seq = o1.map[Seq](NaturalTransformation1.optionToSeq)
    implicitly[o1seq.type <:< (KList { type Cons[X] = Seq[X] })]
    val o1seqa = o1seq.head
    val o1seqb = o1seq.tail.head
    val o1seqc = o1seq.tail.tail.head
    val o1seqd = o1seq.tail.tail.tail.head

    println(l1)
    val l2 = l1.drop(Nat._3)
      println(l2)
    val e0: Int = l1(Nat._0)
    val e2a: Option[Double] = l1.apply(Nat._2)
    val e2b: Option[Double] = l1.drop(Nat._2).head

    val x1 = null : l1.type#Tail#Tail#Tail#Head
    val x2 = null : Nat._3#Fold[HList, ({ type L[X <: HList] = X#Tail })#L, l1.type#Self]#Head
    val x3: Option[Double] = null : l1.type#Drop[Nat._2]#Head

    implicitly[l1.Length <:< Nat._4]
    implicitly[l2.Length <:< Nat._1]
    
    println((l1.length, l2.length))

    import HList._
    val l3a = "foo" |: 42 |: HNil
    val l3b = true |: "baz" |: Some(1.0) |: HNil
    val l3 = l3a |:: l3b
    println(l3 : String |: Int |: Boolean |: String ||: Some[Double])

    val l4 = new HCons(42, new HCons(10.0d, HNil))
    println(l4.getClass)
    println(l4.tail.getClass)
  }
}
