package com.novocode.erased.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.erased._
import syntax._

class BoolTest {
  @Test
  def testBool {
    implicitly[ (False.type# && [False.type]) =:= False.type ]
    implicitly[ (False.type# && [True.type ]) =:= False.type ]
    implicitly[ (True.type#  && [False.type]) =:= False.type ]
    implicitly[ (True.type#  && [True.type ]) =:= True.type  ]

    implicitly[ (False.type# || [False.type]) =:= False.type ]
    implicitly[ (False.type# || [True.type ]) =:= True.type  ]
    implicitly[ (True.type#  || [False.type]) =:= True.type  ]
    implicitly[ (True.type#  || [True.type ]) =:= True.type  ]

    implicitly[ True.type# && [False.type]# IfElse[Any, Int, String] =:= String ]
    implicitly[ True.type# && [True.type ]# IfElse[Any, Int, String] =:= Int    ]
  }
}

object BoolTest1 {
  sealed trait Bool {
    def && (b: Bool): Bool
    def || (b: Bool): Bool
    def ifElse[B](t: => B, f: => B): B
  }

  object True extends Bool {
    def && (b: Bool) = b
    def || (b: Bool) = True
    def ifElse[B](t: => B, f: => B) = t
  }

  object False extends Bool {
    def && (b: Bool) = False
    def || (b: Bool) = b
    def ifElse[B](t: => B, f: => B) = f
  }
}

object BoolTest2 {
  sealed trait Bool {
    type && [B <: Bool] <: Bool
    type || [B <: Bool] <: Bool
    type IfElse[B, T <: B, F <: B] <: B
  }

  object True extends Bool {
    type && [B <: Bool] = B
    type || [B <: Bool] = True.type
    type IfElse[B, T <: B, F <: B] = T
  }

  object False extends Bool {
    type && [B <: Bool] = False.type
    type || [B <: Bool] = B
    type IfElse[B, T <: B, F <: B] = F
  }
}
