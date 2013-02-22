package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import java.util.Arrays

object Test extends App {

   for (b <- List(new B[Int], new B[Long], new B[Double], new B[Any])) {
      
      println("Test expr 1\n" + "-" * 40 + "\n")
      println("Result: " + b.testExpr1)
      println("\n" + "=" * 40 + "\n")

      println("Test expr 2\n" + "-" * 40 + "\n")
      println("Result: " + b.testExpr2)
      println("\n" + "=" * 40 + "\n")

      println("Test expr 3")
      println("Result: " + b.testExpr3)
      println("\n" + "=" * 40 + "\n")

      println("Test expr 4\n" + "-" * 40 + "\n")
      println("Result: " + b.testExpr4)
      println("\n" + "=" * 40 + "\n")

      println("Test expr 5")
      println("Result: " + b.testExpr5)
      println("\n" + "=" * 40 + "\n")

      println("Test expr 6\n" + "-" * 40 + "\n")
      println("Result: " + b.testExpr6)
      println("\n" + "=" * 40 + "\n")

      // TODO: check why test fails when type parameter is not Int
//      println("Test expr 7\n" + "-" * 40 + "\n")
//      println("Result: " + b.testExpr7)
//      println("\n" + "=" * 40 + "\n")

      // TODO: check why test fails when type parameter is not Int
//      println("Test expr 8\n" + "-" * 40 + "\n")
//      println("Result: " + b.testExpr8)
//      println("\n" + "=" * 40 + "\n")
      
      println("\n" + "=" * 40 + "\n")
      println("\n" + "=" * 40 + "\n")
   }
}
