package ch.epfl.lamp.specialized

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.reflect.AnyValManifest

object `package` {

   def specialized[T](expr_f: => Any)(implicit mf: Manifest[T]): Any = macro impl_specialized[T]

   def impl_specialized[T](c: Context)(expr_f: c.Expr[Any])(mf: c.Expr[Manifest[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      import c.universe._

      if (typetagT.tpe == typeOf[Nothing]) {
         // This happens when:
         // 1. specialized {...} is used and there is no type parameter with a manifest in scope 
         // or there is more than one type parameter have manifests and therefore the type could not be inffered.
         c.warning(mf.tree.pos, "specify type parameter using: specialized[T] {...}")
         return expr_f
      }
      // TODO: throw warning if T is a defined type (ex:  specialized[Int] {...} )


      val typeOf_f = expr_f.actualType

      val expr_f_Int = expr_f

      val newExp =
         reify {
            if (mf.splice == manifest[Int]) {
               println("executing Int branch")
               expr_f_Int.splice
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
               expr_f.splice
            }
         }

      val newExpWithCast = c.Expr[Any](TypeApply(Select(newExp.tree, newTermName("asInstanceOf")), List(TypeTree().setType(typeOf_f))))

      c.Expr[Any](Block(List(
         printblockTree(c)("Old expr: " + showRaw(expr_f, printTypes = true)),
         printblockTree(c)(show(expr_f)),
         printblockTree(c)("newExpWithCast: " + showRaw(newExpWithCast, printTypes = true)),
         printblockTree(c)(show(newExpWithCast))),
         newExpWithCast.tree))
      // newExpWithCast
   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str + "\n"))))
   }
}