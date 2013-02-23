package ch.epfl.lamp.specialized

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.reflect.AnyValManifest

object `package` {

   def specialized[T](f: => Any)(implicit mf: Manifest[T]): Any = macro impl_specialized[T]

   def impl_specialized[T](c: Context)(f: c.Expr[Any])(mf: c.Expr[Manifest[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      import c.universe._

      if (typetagT.tpe == typeOf[Nothing]) {
         c.warning(mf.tree.pos, "specify type parameter using: sepezialized[T] {...}")
         return f
      }
      c.warning(mf.tree.pos, "mf.actualType = " + mf.actualType.toString())

      val fInt = f
      val newExp = reify {
         if (mf.splice == manifest[Int]) {
            println("executing Int branch")
            fInt.splice
         } /* else if (mf.splice == manifest[Long]) {
            println("executing Long branch")
            f.splice
         }  else if (mf.splice == manifest[Double]) {
            println("executing Double branch")
            f.splice
         }  else if (mf.splice == manifest[Float]) {
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

      c.Expr[Any](Block(List(
         printblockTree(c)("Old expr: " + showRaw(c.typeCheck(f.tree), printTypes = true)),
         printblockTree(c)(show(f)),
         printblockTree(c)("New expr: " + showRaw(newExp, printTypes = true)),
         printblockTree(c)(show(newExp))),
         newExp.tree))
      //newExp
   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str + "\n"))))
   }
}