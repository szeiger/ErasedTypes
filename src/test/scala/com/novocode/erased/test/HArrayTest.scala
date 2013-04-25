package com.novocode.erased.test

import org.junit.Test
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
}
