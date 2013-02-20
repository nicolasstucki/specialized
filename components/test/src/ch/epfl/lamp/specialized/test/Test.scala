package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._

object Test extends App {

   specialized {
      println("Pjkfsajflo")
   }

   class C[T: Manifest] {
     val arr = new Array[T](1)

     // original:
     def testExpr1 = {
       specialized[T] {
         arr.length
       } + 1
     }

    def testExpr2 = {
       arr(0) = specialized[T] {
         arr(0)
       }
     }

     def testExpr3 = {
       specialized[T] {
         List(arr(0))
       } match {
         case List(x: T) => println(x)
         case _ => ???
       }
     }

     // blind
     def testExpr1unroll1 = {
       arr.length + 1
     }

     def testExpr2unroll1 = {
       arr(0) = arr(0)
     }

     def testExpr3unroll1 = {
       List(arr(0) match {
         case List(x: T) => println(x)
         case _ => ???
       }
     }

     // with manifests
     def testExpr2unroll2 = {
       arr(0) =
         if (manifest[T] == manifest[Int])
           arr.asInstanceOf[Array[Int]](0).asInstanceOf[T]
         else
           ???
     }
   }
}
