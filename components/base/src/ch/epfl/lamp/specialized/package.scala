package ch.epfl.lamp.specialized

import scala.reflect.macros.Context
import scala.language.experimental.macros

object `package` {

   def specializedUnit[T](f: => Unit): Unit = macro impl_specializedUnit[T]

   def impl_specializedUnit[T: c.WeakTypeTag](c: Context)(f: c.Expr[Any]): c.Expr[Unit] = {
      import c.universe._
      val newAST = f.tree
      c.Expr[Unit](Block(List(printlnTree(c)(showRaw(newAST)), printlnTree(c)(show(newAST))), f.tree))
   }

   
   def specialized[T, U](f: => U): U = macro impl_specialized[T, U]

   
   def impl_specialized[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(f: c.Expr[Any]): c.Expr[U] = {
      import c.universe._

      val newAST = f.tree

      c.Expr[U](Block(List(printlnTree(c)(showRaw(newAST)), printlnTree(c)(show(newAST))), f.tree))
   }

   private def printlnTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }
}