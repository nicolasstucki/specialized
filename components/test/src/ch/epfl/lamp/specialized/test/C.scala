package ch.epfl.lamp.specialized.test
import ch.epfl.lamp.specialized._

class C[T: Manifest, U: Manifest] {
   val arr = new Array[T](3)

//   def testExpr0 = {
//      specialized { // should fail
//         arr
//      }
//   }

}

class D[T: Manifest, U] {
   val arr = new Array[T](3)

   def testExpr0 = {
      specialized {
         arr
      }
   }

}