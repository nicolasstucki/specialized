package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import java.util.Arrays

object Test extends App {

   val b = new B[Any]
   b.arr(0) = 0
   b.arr(1) = 7
   b.arr(2) = 2

   println("Test expr 1\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr1)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 2\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr2)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 3")
   println("Result: " + b.testExpr3)
   println("\ns" + "=" * 40 + "\n")

   println("Test expr 4\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr4)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 5")
   println("Results: " + b.testExpr5)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 6\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr6)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 7\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr7)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 8\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr8)
   println("\n" + "=" * 40 + "\n")

   println("Test expr 9\n" + "-" * 40 + "\n")
   println("Result: " + b.testExpr9)
   println("\n" + "=" * 40 + "\n")
}
