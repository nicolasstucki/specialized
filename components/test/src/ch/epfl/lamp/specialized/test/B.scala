package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._

class B[T: Manifest] {
   val arr = new Array[T](3)

   def testExpr1 = {
      specialized[T] {
         arr.length
         ()
      }
   }

   def testExpr2 = {
      specialized[T] {
         arr.length
      } + 1
   }

   def testExpr3 = {
      specialized[T] {
         arr(0)
      } // + 1 // should fail
   }

   def testExpr4 = {
      arr(0) = specialized[T] {
         arr(1)
      }
   }

   def testExpr5 = {
      specialized[T] {
         val arr2 = new Array[T](4)
         arr2
      }
   }

   def testExpr6 = {
      specialized[T] {
         val tup = (arr(0), 4)
         tup
      }
   }

   def testExpr7 = {
      specialized[T] {
         List(arr(0)) match {
            case List(x: T) => println("succes")
            case _          => println("failed")
         }
      }
   }
}
