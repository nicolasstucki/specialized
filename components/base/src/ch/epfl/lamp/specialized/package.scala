package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.AnyValManifest
import scala.reflect.macros.Context

object `package` {

   val warnTypeParameterNeeded = "specify type parameter using: specialized[T] {...}, T must have a manifest"
   val warnSecializedIgnored = "specialized[T] {...} will be ignored (only generic code will be avalible)"
   val warnTypeParameterNotUsed = "code has no reference to type parameter being specialized"

   def specialized[T](expr_f: => Any)(implicit mf: Manifest[T]): Any = macro impl_specialized[T]

   def impl_specialized[T](c: Context)(expr_f: c.Expr[Any])(mf: c.Expr[Manifest[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      import c.universe._

      val typeOf_T = typetagT.tpe
      val typeOf_f = expr_f.actualType

      if (typetagT.tpe == typeOf[Nothing]) {
         // This happens when:
         // 1. specialized {...} is used and there is no type parameter with a manifest in scope 
         // 2. or there is more than one type parameter have manifests and therefore the type could not be inferred.
         c.warning(mf.tree.pos, warnTypeParameterNeeded)
         c.warning(mf.tree.pos, warnSecializedIgnored)
         return c.Expr[Any](Block(List( // TODO remove this return
            printblockTree(c)("expr_f: " + showRaw(expr_f /*, printTypes = true*/ )),
            printblockTree(c)(show(expr_f))),
            expr_f.tree))
         // return expr_f
      }

      // Find whether T is used inside the specialized[T] {...} block. 
      // If T is not used: throws a warning and ignores the specialized
      object traverser extends Traverser {
         var hasT = false
         override def traverse(tree: Tree): Unit = {
            if (tree.tpe.find(_ =:= typeOf_T) != None) hasT = true
            if (!hasT) super.traverse(tree)
         }
      }
      traverser.traverse(expr_f.tree)
      if (!traverser.hasT || typetagT.tpe == typeOf[Any]) {
         c.warning(mf.tree.pos, warnTypeParameterNotUsed)
         c.warning(mf.tree.pos, warnSecializedIgnored)
         return c.Expr[Any](Block(List( // TODO remove this return
            printblockTree(c)("expr_f: " + showRaw(expr_f /*, printTypes = true*/ )),
            printblockTree(c)(show(expr_f))),
            expr_f.tree))
         // return expr_f
      }

      // CREATE SPECIFIC VARIANTS OF TREE
      def subs(tree0: Tree, newType: Type): Tree = {
         def rec(tree: Tree): Tree = {
            tree match {
               case Block(trees, last)      => Block(trees map (rec(_)), rec(last))
               case Apply(func, params)     => Apply(rec(func), params map (rec(_)))
               case TypeApply(func, params) => TypeApply(rec(func), params map (rec(_)))
               case Select(term, name)      => Select(rec(term), name)
               case typeTree: TypeTree      => typeTree //if (tpeTree.tpe == typeOf_T) TypeTree(newType) else tpeTree
               // case ValDef(mod, name, tpt, rhs) => ValDef(mod, name, tpt, rhs)
               case x                       => tree
            }
         }
         rec(tree0)
      }

      def cast(expr: c.Expr[Any], tpe: Type): c.Expr[Any] = c.Expr[Any](TypeApply(Select(expr.tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe))))

      // TODO: add casts to specific implementation (ex. Array[T] to Array[Int] with .asInstanceOf[Array[Int]])
      // tree.tpe.find(_ =:= typeOf_T) might be useful.
      val expr_f_Int = c.Expr[Any](subs(expr_f.tree.duplicate, typeOf[Int]))

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      // TODO: create alternative variant selection (try with match{})
      val newExpr =
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

      val newExprWithCast = cast(newExpr, typeOf_f)

      // RETURN THE TREE
      return c.Expr[Any](Block(List( // TODO remove this return
         printblockTree(c)("expr_f: " + showRaw(expr_f /*, printTypes = true*/ )),
         printblockTree(c)(show(expr_f)),
         printblockTree(c)("\n" + "newExprWithCast: " + showRaw(newExprWithCast /*, printTypes = true*/ )),
         printblockTree(c)(show(newExprWithCast))),
         newExprWithCast.tree))
      //return newExpr

   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }

}