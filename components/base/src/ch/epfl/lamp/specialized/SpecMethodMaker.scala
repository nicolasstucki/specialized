package ch.epfl.lamp.specialized

import scala.reflect.ClassTag
import scala.reflect.macros.Context

object SpecMethodMaker {

   def createSpecMethod[T](c: Context)(classTag: c.Expr[ClassTag[T]], typeOf_T: c.Type, body: c.Expr[Any]): c.Expr[Any] = {
      import c.universe._
      val typeOf_Spec = Ident(newTypeName("U")).tpe
      val newBody = c.Expr[Any](specBody(c)(typeOf_T, typeOf_Spec, body.tree)(classTag))
      val specCallers = createSpecCallers(c)(classTag)

      c.warning(classTag.tree.pos, showRaw(reify({ def spec[@specialized U](arr: Array[U]) = { newBody.splice }; specCallers.splice })))

      reify({ def spec[@specialized U](arr: Array[U]) = { newBody.splice }; specCallers.splice })
   }

   private def createSpecCallers[T](c: Context)(classTag: c.Expr[ClassTag[T]]) = {
      import c.universe._
      val callIntSpec = reify(Unit)
      val callDoubleSpec = reify(Unit)
      val callBooleanSpec = reify(Unit)
      val callGenSpec = reify(Unit)
//      Apply(
//				TypeApply(
//					Ident(newTermName("spec")), 
//					List(TypeTree())), 
//				List(
//					TypeApply(
//						Select(
//							Select(
//								This(
//									newTypeName("TestArray2Reverse")), 
//								newTermName("arr")), 
//							newTermName("asInstanceOf")), 
//						List(
//							TypeTree().setOriginal(
//								AppliedTypeTree(Select(Ident(scala), scala.Array), 
//									List(TypeTree().setOriginal(Select(Ident(scala), scala.Any))))))
//						)
//					)
//				)
      
      reify(
         {
            if (classTag.splice == manifest[Int]) {
               callIntSpec.splice
            } else if (classTag.splice == manifest[Double]) {
               callDoubleSpec.splice
            } else if (classTag.splice == manifest[Boolean]) {
               callBooleanSpec.splice
            } else {
               callGenSpec.splice
            }
         })
   }

   private def specBody[T](c: Context)(typeOf_T: c.Type, typeOf_Spec: c.Type, tree0: c.Tree)(classTag: c.Expr[ClassTag[T]]): c.Tree = {
      import c.universe._
      def rec(tree: Tree): Tree = {
         tree match {
            case block @ Block(trees, last)          => Block(trees map (rec(_)), rec(last))
            case apply @ Apply(func, params)         => Apply(rec(func), params map (rec(_)))
            case typeApply @ TypeApply(func, params) => TypeApply(rec(func), params map (rec(_)))
            case select @ Select(term, name) =>
               if (name == newTermName("arr")) Ident(newTermName("arr"))
               else Select(rec(term), name)
            case typeTree: TypeTree => typeTree
            case valDef @ ValDef(mod, name, tpt, rhs) => {
               val newType = ReType.reType(c)(typeOf_T, typeOf_Spec, tpt.tpe.widen)
               ValDef(mod, name, newType, rec(rhs))
            }
            case lit: Literal                 => lit
            case ths: This                    => ths
            case ident: Ident                 => Ident(ident.name)
            case mtch @ Match(param, classes) => mtch
            case x                            => tree
         }
      }
      rec(tree0)
   }
}