package ch.epfl.lamp.specialized.test

import ch.epfl.lamp.specialized._
import java.util.Arrays

object Test extends App {

   val b = new B[Int]
   for (i <- 1 until b.arr.length) b.arr(i) = i

   printresult(1, b.testExpr1)
   printresult(2, b.testExpr2)
   printresult(3, b.testExpr3)
   printresult(4, b.testExpr4)
   printresult(5, b.testExpr5)
   printresult(6, b.testExpr6)
   printresult(7, b.testExpr7)
   printresult(8, b.testExpr8)
   printresult(9, b.testExpr9)
   printresult(10, b.testExpr10)

   def printresult(n: Int, result: => Any) = {
      println("\n" + ("=" * 150 + "\n") * 3)
      println("Test expr " + n + "\n" + ("-" * 13) + "\n")
      println("Result: " + result)
   }
}
