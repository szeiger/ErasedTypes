package com.novocode.erased.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.erased._
import HList._

class HArrayTest {
  @Test
  def testHArray {
    val v1 = HArray("foo", 42, true)
    val v1t: HArray[String |: Int ||: Boolean] = v1
    println(v1t)

    // Access by Nat index (0-based)
    println(v1(Nat._0): String)
    println(v1(Nat._1): Int)
    println(v1(Nat._2): Boolean)

    // Access by macro-generated accessors (1-based)
    println(v1._1: String)
    println(v1._2: Int)
    println(v1._3: Boolean)
  }

  @Test
  def testVarargs {
    // This is in a separate method due to SI-7420
    val h0 = HArray()
    println(h0)
    val h1 = HArray(Seq("foo", 42, true): _*)
    println(h1)
  }

  @Test
  def testVariance {
    val h = HArray("foo", 42, true)

    def f1(h: HArray[String |: Int ||: _]) = HArray(h._1, h._2)
    def f2(h: HArray[String |: Int ||: Any]) = HArray(h._1, h._2)
    def f3(h: HArray[String |: HList]) = h._1

    val r1 = f1(h)
    val r2 = f2(h)
    val r3 = f3(h)

    val r1t: HArray[String ||: Int] = r1
    val r2t: HArray[String ||: Int] = r2
    val r3t: String = r3

    assertEquals(HArray("foo", 42), r1)
    assertEquals(HArray("foo", 42), r2)
    assertEquals("foo", r3)
  }

  @Test def testSpecialization {
    val h1 = HArray("foo", 42, true)
    val h2 = HArray("foo", 42)

    val h1t: HArrayA[String |: Int ||: Boolean] = h1
    val h2t: HArray2[String, Int] = h2

    val i: Int = h2t._2
    assertEquals(42, i)
  }
}
