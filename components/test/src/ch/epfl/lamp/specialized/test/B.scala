package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import scala.unchecked
import scala.reflect.ClassTag

class B[T: ClassTag] {
   val size = 5
   val arr = new Array[T](size)

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
      specialized[T] { // should fail with +1 the after it
         arr(0)
      } // + 1 
   }
//   def testExpr3unrolled = {
//      if (ClassTag[T] == manifest[Manifest[Int]]) {
//         arr.asInstanceOf[Array[Int]](0)
//      } else {
//         arr(0)
//      }.asInstanceOf[T]
//   }

   def testExpr4 = {
      arr(0) = specialized[T] {
         arr(1)
      }
   }
//   def testExpr4unrolled = {
//      if (manifest[T] == manifest[Manifest[Int]]) {
//         arr.asInstanceOf[Array[Int]](0) = arr.asInstanceOf[Array[Int]](1)
//      } else {
//         arr(0) = arr(1)
//      }.asInstanceOf[Unit]
//   }

   def testExpr5 = {
      specialized[T] {
         val arr2 = new Array[T](4)
         arr2(0) = arr(0)
         arr2
      }
   }
//   def testExpr5unrolled = {
//      if (manifest[T] == manifest[Manifest[Int]]) {
//         val arr2: Array[Int] = new Array[Int](4)
//         arr2(0) = arr.asInstanceOf[Array[Int]](1)
//         arr2
//      } else {
//         val arr2: Array[T] = new Array[T](4)
//         arr2(0) = arr(0)
//         arr2
//      }.asInstanceOf[Array[T]]
//   }

   def testExpr6 = {
      specialized[T] {
         val tup = (arr(1), 4)
         tup
      }
   }
//   def testExpr6unrolled = {
//      if (manifest[T] == manifest[Manifest[Int]]) {
//         val tup: Tuple2[Int, Int] = (arr.asInstanceOf[Array[Int]](1), 4)
//         tup
//      } else {
//         val tup: Tuple2[T, Int] = (arr(1), 4)
//         tup
//      }.asInstanceOf[Tuple2[T, Int]]
//   }

   def testExpr7 = {
      specialized[T] {
         (arr(0), 11)
      }
   }
//   def testExpr7unrolled = {
//      if (manifest[T] == manifest[Manifest[Int]]) {
//         (arr.asInstanceOf[Array[Int]](0), arr.asInstanceOf[Array[Int]](1))
//      } else {
//         (arr(0), arr(1))
//      }
//   }

   def testExpr8 = {
      specialized[T] {
         List(arr(2)) match {
            case (x: T) :: tail => println("matched: (x: T) :: tail")
            case head :: tail   => println("matched: head :: tail")
            case Nil            => println("match: Nil")
         }
      }
   }
//   def testExpr8unrolled = {
//      if (manifest[T] == manifest[Manifest[Int]]) {
//         List(arr.asInstanceOf[Array[Int]](2)) match {
//            case (x: Int) :: tail => println("matched: (x: T) :: tail")
//            // case head :: tail     => println("matched: head :: tail") // unreachable code, compiler will be able to eliminate later on
//            case Nil              => println("match: Nil")
//         }
//      } else {
//         List(arr(2)) match {
//            case (x: T) :: tail => println("matched: (x: T) :: tail")
//            case head :: tail   => println("matched: head :: tail")
//            case Nil            => println("match: Nil")
//         }
//      }
//   }

   def testExpr9 = {
//      specialized[T] { // should warn and ignore
//         val n = 3
//         n + 2
//      }
   }
   def testExpr9unroled = {
      val n: Int = 3
      n + 2
   }

   def testExpr10 = {
//      specialized {
//                   for (index <- 0 until (arr.length/2)) { // FIXME: this should work
//                     val temp = arr(index)
//                     arr(index) = arr(arr.length - index - 1)
//                     arr(arr.length - index - 1) = temp
//                  }
//      }
   }
}
