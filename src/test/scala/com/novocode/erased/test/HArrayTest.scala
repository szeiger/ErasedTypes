package com.novocode.erased.test

import org.junit.Test
import com.novocode.erased._

class HArrayTest {
  @Test
  def testHArray {
    val v1 = HArray("foo", 42, true)
    println(v1)

    // Access by Nat index (0-based)
    println(v1(Nat._0): String)
    println(v1(Nat._1): Int)
    println(v1(Nat._2): Boolean)

    // Access by macro-generated accessors (1-based)
    println(v1._1: String)
    println(v1._2: Int)
    println(v1._3: Boolean)

    // Ensure that the accessors generate stable paths
    import v1._1.length
    ()
  }
}
