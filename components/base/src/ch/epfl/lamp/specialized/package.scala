package ch.epfl.lamp.specialized

import scala.reflect.macros.Context
import scala.language.experimental.macros

object `package` {

   def specialized[T](f: => Any)(implicit mf: Manifest[T]): Any = macro impl_specialized[T]

   def impl_specialized[T: c.WeakTypeTag](c: Context)(f: c.Expr[Any])(mf: c.Expr[Manifest[T]]): c.Expr[Any] = {
      import c.universe._

      val Select(_, mfTermName) = mf.tree
      if (mfTermName == newTermName("Nothing")) c.error(mf.tree.pos, "specify type parameter using: sepezialized[T] {...}")

      val newExp = reify {
         if (mf.splice == manifest[Int]) {
            println("executing => Int")
            f.splice
         } else if (mf.splice == manifest[Long]) {
            println("executing => Long")
            f.splice
         } else if (mf.splice == manifest[Double]) {
            println("executing => Double")
            f.splice
         } else if (mf.splice == manifest[Float]) {
            println("executing => Float")
            f.splice
         } else if (mf.splice == manifest[Short]) {
            println("executing => Short")
            f.splice
         } else if (mf.splice == manifest[Byte]) {
            println("executing => Byte")
            f.splice
         } else if (mf.splice == manifest[Char]) {
            println("executing => Char")
            f.splice
         } else if (mf.splice == manifest[Unit]) {
            println("executing => Unit")
            f.splice
         } else if (mf.splice == manifest[Boolean]) {
            println("executing => Boolean")
            f.splice
         } else {
            println("executing => Gen")
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