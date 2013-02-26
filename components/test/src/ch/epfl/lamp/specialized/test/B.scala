package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import scala.unchecked

class B[T: Manifest] {
   val arr = new Array[T](3)

   def testExpr1 = {
      specialized[T] { // should warn and ignore
         arr.length
         ()
      }
   }
   def testExpr1unrolled = {
      arr.length
      ()
   }

   def testExpr2 = {
      specialized[T] { // should warn and ignore
         arr.length
      } + 1
   }
   def testExpr2unrolled = {
      arr.length + 1
   }

   def testExpr3 = {
      specialized[T] {
         arr(0)
      } //+ 1 // should fail
   }
   def testExpr3unrolled = {
      if (manifest[T] == manifest[Manifest[Int]]) {
         arr.asInstanceOf[Array[Int]](0)
      } else {
         arr(0)
      }
   }

   def testExpr4 = {
      arr(0) = specialized[T] {
         arr(1)
      }
   }
   def testExpr4unrolled = {
      if (manifest[T] == manifest[Manifest[Int]]) {
         arr.asInstanceOf[Array[Int]](0) = arr.asInstanceOf[Array[Int]](1)
      } else {
         arr(0) = arr(1)
      }
   }

   def testExpr5 = {
      specialized[T] {
         val arr2 = new Array[T](4)
         arr2
      }
   }
   def testExpr5unrolled = {
      if (manifest[T] == manifest[Manifest[Int]]) {
         val arr2 = new Array[Int](4)
         arr2
      } else {
         val arr2 = new Array[T](4)
         arr2
      }
   }

   def testExpr6 = {
      specialized[T] {
         val tup = (arr(1), 4)
         tup
      }
   }
   def testExpr6unrolled = {
      if (manifest[T] == manifest[Manifest[Int]]) {
         val tup = (arr.asInstanceOf[Array[Int]](1), 4)
         tup
      } else {
         val tup: (T, Int) = (arr(1), 4)
         tup
      }
   }

   def testExpr7 = {
      specialized[T] {
         (arr(0), 11)
      }
   }
   def testExpr7unrolled = {
      if (manifest[T] == manifest[Manifest[Int]]) {
         (arr.asInstanceOf[Array[Int]](0), arr.asInstanceOf[Array[Int]](1))
      } else {
         (arr(0), arr(1))
      }
   }

   def testExpr8 = {
      specialized[T] {
         arr(2) :: Nil match {
            case (x: T) :: tail => println("matched: (x: T) :: tail")
            case head :: tail   => println("matched: head :: tail")
            case Nil            => println("match: Nil")
         }
      }
   }
   def testExpr8unrolled = {
      if (manifest[T] == manifest[Manifest[Int]]) {
         List(arr.asInstanceOf[Array[Int]](2)) match {
            case (x: Int) :: tail => println("matched: (x: T) :: tail")
            // case head :: tail     => println("matched: head :: tail") // unreachable code, compiler will be able to eliminate later on
            case Nil              => println("match: Nil")
         }
      } else {
         List(arr(2)) match {
            case (x: T) :: tail => println("matched: (x: T) :: tail")
            case head :: tail   => println("matched: head :: tail")
            case Nil            => println("match: Nil")
         }
      }
   }

   def testExpr9 = {
      specialized[T] { // should warn and ignore
         val n = 3
         n + 2
      }
   }
   def testExpr9unroled = {
      val n = 3
      n + 2
   }
}
