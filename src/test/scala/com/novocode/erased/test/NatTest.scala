package com.novocode.erased.test

import org.junit.Test
import com.novocode.erased.Nat._

class NatTest {
  @Test
  def testNat {
    println( (_2 + _2): _4 )
    println( (_3 + _0): _3 )
    println( (_0 + _2): _2 )
    println( (_1 + _2): _3 )
    println( (_2 * _3): _6 )
    println( (_2 ^ _3): _8 )
    println( (_3 ^ _2): _9 )
    println( _1._0: _10 )
    println( _1._6: (_8 # * [_2]) )
    
    implicitly[_1._6 =:= (_4 # * [_4])]
    implicitly[(_1._5 # ++) =:= (_4 # * [_4])]
    implicitly[(_1 # * [_10] # + [_6]) =:= (_4 # * [_4])]
    
    val x: List[List[List[String]]] = (null: _3#Fold[Any, List, String])
  }
}
