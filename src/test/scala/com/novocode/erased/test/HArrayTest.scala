package com.novocode.erased.test

import org.junit.Test
import com.novocode.erased._

class HArrayTest {
  @Test
  def testHArray {
    val v1 = HArray("foo", 42, true)
    println(v1)
    println(v1(Nat._0): String)
    println(v1(Nat._1): Int)
    println(v1(Nat._2): Boolean)
  }
}
