package ch.epfl.lamp.specialized

import scala.reflect.macros.Context
import scala.language.experimental.macros

object `package` {

   @deprecated
   def specializedUnit[T](f: => Unit): Unit = macro impl_specializedUnit[T]

   @deprecated
   def impl_specializedUnit[T: c.WeakTypeTag](c: Context)(f: c.Expr[Any]): c.Expr[Unit] = {
      import c.universe._
      val newAST = f.tree
      c.Expr[Unit](Block(List(printlnTree(c)(showRaw(newAST)), printlnTree(c)(show(newAST))), f.tree))
   }

   //TODO add manifest to T
   def specialized[T](f: => Any): Any = macro impl_specialized[T]

   def impl_specialized[T: c.WeakTypeTag](c: Context)(f: c.Expr[Any]): c.Expr[Any] = {
      import c.universe._
      
      //val typeOfOutput = f.actualType.toString()
      
      val newAST = f.tree

      c.Expr[Any](Block(List(printlnTree(c)(showRaw(newAST)), printlnTree(c)(show(newAST))), f.tree))
   }

   private def printlnTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }
}