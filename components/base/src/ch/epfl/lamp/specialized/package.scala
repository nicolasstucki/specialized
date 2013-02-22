package ch.epfl.lamp.specialized

import scala.reflect.macros.Context
import scala.language.experimental.macros

object `package` {

   def specialized[T](f: => Any)(implicit mf: Manifest[T]): Any = macro impl_specialized[T]

   def impl_specialized[T: c.WeakTypeTag](c: Context)(f: c.Expr[Any])(mf: c.Expr[Manifest[T]]): c.Expr[Any] = {
      import c.universe._

      val Select(_, mfTermName) = mf.tree
      if (mfTermName == newTermName("Nothing")) c.error(mf.tree.pos, "specify type parameter using: sepezialized[T] {...}")

      // val newExp = c.Expr[Any](f.tree)
      val newExp = reify {
         if (mf.splice == manifest[Int]) {
            println("executing Int branch")
            f.splice
         } else if (mf.splice == manifest[Long]) {
            println("executing Long branch")
            f.splice
         } else if (mf.splice == manifest[Double]) {
            println("executing Double branch")
            f.splice
         } /* else if (mf.splice == manifest[Float]) {
                  println("executing Float branch")
                  f.splice
               } else if (mf.splice == manifest[Short]) {
                  println("executing Short branch")
                  f.splice
               } else if (mf.splice == manifest[Byte]) {
                  println("executing Byte branch")
                  f.splice
               } else if (mf.splice == manifest[Char]) {
                  println("executing Char branch")
                  f.splice
               } else if (mf.splice == manifest[Unit]) {
                  println("executing Unit branch")
                  f.splice
               } else if (mf.splice == manifest[Boolean]) {
                  println("executing Boolean branch")
                  f.splice
               }*/ else {
            println("executing Gen branch")
            f.splice
         }
      }

      c.Expr[Any](Block(List(printlnTree(c)(showRaw(newExp)), printlnTree(c)(show(newExp))), newExp.tree))
      //newExp
   }

   private def printlnTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }
}