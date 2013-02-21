package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import java.util.Arrays

object Test extends App {

   val v = new C[Long, Double]
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

   val v2 = new D[List[Long]]
   v2.arr(0) = 1 :: Nil
   v2.arr(1) = 2 :: Nil
   v2.arr(2) = 3 :: Nil
   println("Test expr A")
   println(v2.testExprA)
   println

   class D[T: Manifest] {
      val arr = new Array[T](3)
      def testExprA = {
         specialized[T] {
            arr.length
         } + specialized {
            arr.length
         } + 1000;
      }
   }

   class C[T: Manifest, U: Manifest] {
      val arr = new Array[T](3)

      // original:
      def testExpr0 = {
         specialized[T] {
            val tup = (arr(0), arr(2))
            val tupII: Tuple2[Int, Int] = (5, 7)
            println("testExpr0 message: " + tup)
         }
      }

      def testExpr1 = {
         specialized[T] {
            arr.length
         } + 1000;

         //  this should fail
         //         specialized { 
         //            arr.length
         //         }
      }

      def testExpr2 = {
         arr(0) = specialized[U] {
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
