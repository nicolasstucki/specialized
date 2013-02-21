package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import java.util.Arrays

object Test extends App {

   val v = new C[Int]
   v.arr(0) = 1
   v.arr(1) = 2
   v.arr(2) = 3
   println("Test expr 0")
   println(v.testExpr0)
   println

   println("Test expr 1")
   println(v.testExpr1)
   println

   println("Test expr 2")
   println(v.testExpr2)
   println

   //println( v.testExpr3 )

   class C[T: Manifest] {
      val arr = new Array[T](3)

      // original:
      def testExpr0 = {
         specialized[T] {
            val tup = (arr(0), arr(2))
            val tupII = (5, 7)
            println("testExpr0 message: " + tup)
         }
      }

      def testExpr1 = {
         specialized[T] {
            arr.length
         } + specialized { // TODO this should fail
            arr.length
         } + 1000;
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
            case _          => ???
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
         List(arr(0)) match {
            case List(x: T) => println(x)
            case _          => ???
         }
      }

      //     // with manifests
      //     def testExpr2unroll2 = {
      //       arr(0) =
      //         if (manifest[T] == manifest[Int])
      //           arr.asInstanceOf[Array[Int]](0).asInstanceOf[T]
      //         else
      //           ???
      //     }
   }
}
