package ch.epfl.lamp.specialized.test
import ch.epfl.lamp.specialized._

class A[T] {
   val arr = new Array[Any](3)

   def testExpr1 = {
      specialized { // should warn and ignore
         arr
      }
   }

   //      def testExpr2 = {
   //         specialized[T] { // should fail
   //            arr
   //         }
   //      }

   def testExpr3 = {
      specialized[Int] { // should warn and ignore
         arr
      }
   }
   def testExpr4 = {
      specialized[Long] { // should warn and ignore
         arr
      }
   }
   def testExpr5 = {
      specialized[Any] { // should warn and ignore
         arr
      }
   }
   def testExpr6 = {
      specialized[Nothing] { // should warn and ignore
         arr
      }
   }
   def testExpr7 = {
      specialized[Unit] { // should warn and ignore
         arr
      }
   }
   def testExpr8 = {
      specialized[BigInt] { // should warn and ignore
         arr
      }
   }
}