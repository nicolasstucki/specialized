package ch.epfl.lamp.specialized.test
import ch.epfl.lamp.specialized._

class A {
   val arr = new Array[Any](3)

   def testExpr1 = {
      specialized { // should fail
         arr
      }
   }

   //   def testExpr2 = {
   //      specialized[T] { // should fail
   //         arr
   //      }
   //   }

   def testExpr3 = {
      specialized[Int] { // should fail
         arr
      }
   }
   def testExpr4 = {
      specialized[Long] { // should fail
         arr
      }
   }
   def testExpr5 = {
      specialized[Any] { // should fail
         arr
      }
   }
}