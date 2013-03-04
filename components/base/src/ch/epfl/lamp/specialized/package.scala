package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.AnyValManifest
import scala.reflect.macros.Context
import scala.reflect.ClassTag

object `package` {


   def specialized[T](expr_f: => Any)(implicit mf: ClassTag[T]): Any = macro impl_specialized[T]

   def impl_specialized[T](c: Context)(expr_f: c.Expr[Any])(mf: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      import c.universe._

      // TYPE PARAMETER CHECKS
      val typeOf_T = typetagT.tpe
      val typeOf_f = expr_f.actualType

      // Check if T is a valid type parameter
      typeOf_T match {
         case TypeRef(NoPrefix, _, _) => // Do nothing, typeOf_T is OK
         case TypeRef(_, _, _) => {
            // This happens when:
            // 1. specialized {...} is used and there is no type parameter with a ClassTag in scope 
            // 2. or there is more than one type parameter have manifests and therefore the type could not be inferred.
            // 3. The type parameter is not a type parameter of the enclosing context, examples: specialized[Int] {...}, specialized[Any] {...}, specialized[Array[Int]] {...}, specialized[T] {...}, ...   
            c.warning(mf.tree.pos, "Specify type parameter using: specialized[T] {...}, T must be a type parameter of the enclosing context and it must have a ClassTag. Type patameter must be on top level, example: if you want to specialize an Array[T] use specialize[T] {...}.")
            return expr_f
         }
      }

      // CREATE SPECIFIC VARIANTS OF TREE
      def reTypeTree(from: Type, to: Type, in: Type): Tree = TypeTree().setType(in.substituteTypes(List(from.typeSymbol), List(to)))
      def cast(tree: Tree, tpe: Type): Tree = TypeApply(Select(tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe)))
      def subs(tree0: Tree, newType: Type): Tree = {
         def rec(tree: Tree): Tree = {
            tree match {
               case Block(trees, last)                   => Block(trees map (rec(_)), rec(last))
               case Apply(func, params)                  => Apply(rec(func), params map (rec(_)))
               case TypeApply(func, params)              => TypeApply(rec(func), params map (rec(_)))
               case t @ Select(term, name)               => Select(rec(term), name)
               case typeTree: TypeTree                   => typeTree // if (typeTree.tpe =:= typeOf_T) TypeTree(newType) else typeTree
               case valDef @ ValDef(mod, name, tpt, rhs) => ValDef(mod, name, tpt, rhs)
               case lit: Literal                         => lit
               case ths: This                            => ths
               case id: Ident                            => Ident(id.name)
               case mtch @ Match(param, classes)         => mtch
               case x                                    => { /*c.warning(mf.tree.pos, ">>> match def missing: " + showRaw(x))*/ ; tree }
            }
         }
         rec(tree0)
      }

      // TODO: add casts to specific implementation (ex. Array[T] to Array[Int] with .asInstanceOf[Array[Int]])
      // check if it is needed to manually wrap the blocks returning type if it is of type T 
      // tree.tpe.find(_ =:= typeOf_T) might be useful.
      val expr_f_Int = c.Expr[Any](subs(expr_f.tree.duplicate, typeOf[Int]))
      val expr_f_Long = c.Expr[Any](subs(expr_f.tree.duplicate, typeOf[Long]))

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      // TODO: create alternative variant selection (try with match{})
      val newExpr =
         reify {
            if (mf.splice == manifest[Int]) {
               println("executing Int branch")
               expr_f_Int.splice
            } else if (mf.splice == manifest[Long]) {
               println("executing Long branch")
               expr_f_Long.splice
            } /* else if (mf.splice == manifest[Double]) {
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

      val newExprWithCast: c.Expr[T] = c.Expr(cast(newExpr.tree, typeOf_f))

      // RETURN THE TREE
      return c.Expr(Block(List( // TODO remove this return
         printblockTree(c)("expr_f: " + showRaw(expr_f.tree)),
         printblockTree(c)(show(expr_f.tree)),
         printblockTree(c)("\n" + "newExprWithCast: " + showRaw(newExprWithCast.tree, printTypes = true)),
         printblockTree(c)(show(newExprWithCast.tree, printTypes = true) + "\n")),
         newExprWithCast.tree))
      //return newExpr

   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }

}