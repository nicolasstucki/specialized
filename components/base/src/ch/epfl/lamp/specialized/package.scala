package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.AnyValManifest
import scala.reflect.ClassTag
import scala.reflect.macros.Context

/**
 * @author Nicolas Stucki
 *
 */
object `package` {

   /**
    * Specialized block
    * @param expr_f: Code inside the specialized block
    * @param classTag: Implicit ClassTag of the type being specialized
    * @return: Original return statement of the block
    */
   def specialized[T](expr_f: => Any)(implicit classTag: ClassTag[T]): Any = macro impl_specialized[T]

   /**
    * Macro implementation of specialized[T]{...}
    * @param c: Context
    * @param expr_f: Code inside the specialized block
    * @param classTag: ClassTag of the type being specialized
    * @param typetagT: Implicit WeakTypeTag of the type being specialized
    * @return: Code inside the specialized block with specialization
    */
   def impl_specialized[T](c: Context)(expr_f: c.Expr[Any])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
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
            c.warning(classTag.tree.pos, "Specify type parameter using: specialized[T] {...}, T must be a type parameter of the enclosing context and it must have a ClassTag." +
               " Type patameter must be on top level, example: if you want to specialize an Array[T] use specialize[T] {...}.")
            return expr_f
         }
      }

      // CREATE SPECIFIC VARIANTS OF TREE
      def cast(tree: Tree, tpe: Type): Tree = TypeApply(Select(tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe)))
      def subs(tree0: Tree, newType: Type): Tree = {
         @inline def printw(name: String, t: Tree) = Unit //c.warning(ct.tree.pos, ">> " + name + ": <<<" + showRaw(t.tpe) + ">>> " + showRaw(t)) // TODO: remove this (debug) 
         def rec(tree: Tree): Tree = {
            tree match {
               case block @ Block(trees, last) => {
                  printw("block", block)
                  Block(trees map (rec(_)), rec(last))
               }
               case apply @ Apply(func, params) => {
                  printw("apply", apply)
                  Apply(rec(func), params map (rec(_)))
               }
               case typeApply @ TypeApply(func, params) => {
                  printw("typeApply", typeApply)
                  TypeApply(rec(func), params map (rec(_)))
               }
               case select @ Select(term, name) => {
                  printw("select", select)
                  Select(rec(term), name)
               }
               case typeTree: TypeTree => {
                  printw("typeTree", typeTree)
                  typeTree
               }
               case valDef @ ValDef(mod, name, tpt, rhs) => {
                  printw("valDef", valDef)
                  ValDef(mod, name, tpt, rhs)
               }
               case lit: Literal => lit
               case ths: This    => ths
               case ident: Ident => {
                  //                  val tpe = ident.tpe.widen.substituteSymbols(List(typeOf_T.typeSymbol), List(newType.typeSymbol))
                  //                  val newIdent = cast(Ident(ident.name), tpe)
                  printw("ident", ident)
                  //                  printw("newIdent", newIdent)
                  Ident(ident.name)
               }
               case mtch @ Match(param, classes) => {
                  printw("mtch", mtch)
                  mtch
               }
               case x => {
                  /*c.warning(ct.tree.pos, ">>> match def missing: " + showRaw(x))*/
                  tree
               }
            }
         }
         rec(tree0)
      }

      // RETRIEVE DEVINITIONS AND USES OF TERMS THAT HAVE T IN THE TYPE
      val defDefs = SpecTraversers.getDefDefsWithTypeParam(c)(expr_f, typeOf_T)
      val idents = SpecTraversers.getIdentsWithTypeParam(c)(expr_f, typeOf_T)
      val selectTypeRefs = SpecTraversers.getSelectTypeRefWithTypeParam(c)(expr_f, typeOf_T)
      val valDefs = SpecTraversers.getValDefsWithTypeParam(c)(expr_f, typeOf_T)

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      val specMeth = SpecMethodMaker.createSpecMethod(c)(classTag, typeOf_T, expr_f)
      val newExpr = c.Expr(cast(specMeth.tree, typeOf_f))

      // RETURN THE NEW TREE
      c.warning(classTag.tree.pos, showRaw(expr_f) )
      newExpr
   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }

}