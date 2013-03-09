package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.AnyValManifest
import scala.reflect.ClassTag
import scala.reflect.macros.Context

object `package` {

   def specialized[T](expr_f: => Any)(implicit classTag: ClassTag[T]): Any = macro impl_specialized[T]

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
            c.warning(classTag.tree.pos, "Specify type parameter using: specialized[T] {...}, T must be a type parameter of the enclosing context and it must have a ClassTag. Type patameter must be on top level, example: if you want to specialize an Array[T] use specialize[T] {...}.")
            return expr_f
         }
      }

      // IDENTIFY VALs, VARs, DEFs DEFINITIONS AND USES IN BLOCK 
      class traverser extends c.universe.Traverser {
         var defdefs = Set.empty[DefDef]
         var idents = Set.empty[Ident]
         var selectTypeRefs = Set.empty[Select]
         var valdefs = Set.empty[ValDef]
         
         override def traverse(tree: Tree): Unit = {
            @inline def checkTpe(tree: Tree): Boolean = tree.tpe.widen.exists(_ == typeOf_T)
            tree match {
               case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if checkTpe(tpt) => defdefs = defdefs + defdef
               case ident: Ident if checkTpe(ident) => idents = idents + ident
               case select: Select if checkTpe(select) =>
                  select.tpe.widen match {
                     case _: MethodType =>
                     case _: TypeRef    => selectTypeRefs = selectTypeRefs + select
                     case _             =>
                  }
               case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) => valdefs = valdefs + valdef
               case _ =>
            }
            super.traverse(tree)
         }
         
         def defdefNames: Set[Name] = defdefs map { case DefDef(_, name, _, _, _, _) => name }
         def identNames: Set[Name] = idents map { case Ident(name) => name }
         def selectTypeRefNames: Set[Name] = selectTypeRefs map { case Select(_, name) => name }
         def valdefNames: Set[Name] = valdefs map { case ValDef(_, name, _, _) => name }
      }
      val tra = new traverser
      tra.traverse(expr_f.tree)

//      c.warning(classTag.tree.pos, "defdefs ::>>> " + tra.defdefs.map(showRaw(_)))
//      c.warning(classTag.tree.pos, "idents ::>>> " + tra.idents.map(showRaw(_)))
//      c.warning(classTag.tree.pos, "selectsTypeRef ::>>> " + tra.selectTypeRefs.map(showRaw(_)))
//      c.warning(classTag.tree.pos, "valdefs ::>>> " + tra.valdefs.map(showRaw(_)))

      c.warning(classTag.tree.pos, "defdefNames ::>>> " + tra.defdefNames.map(showRaw(_)))
      c.warning(classTag.tree.pos, "identNames ::>>> " + tra.identNames.map(showRaw(_)))
      c.warning(classTag.tree.pos, "selectsTypeRefName ::>>> " + tra.selectTypeRefNames.map(showRaw(_)))
      c.warning(classTag.tree.pos, "valdefNames ::>>> " + tra.valdefNames.map(showRaw(_)))

      
      // CREATE SPECIFIC VARIANTS OF TREE
      def reTypeTree(from: Type, to: Type, in: Type): Tree = TypeTree().setType(in.substituteTypes(List(from.typeSymbol), List(to)))
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
                  //TODO: if 
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
                  val tpe = ident.tpe.widen.substituteSymbols(List(typeOf_T.typeSymbol), List(newType.typeSymbol))
                  val newIdent = cast(Ident(ident.name), tpe)
                  // c.warning(ct.tree.pos, "*****" + showRaw(tpe.normalize) )
                  printw("ident", ident)
                  printw("newIdent", newIdent)

                  //Ident(ident.name)
                  newIdent
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

      // TODO: add casts to specific implementation (ex. Array[T] to Array[Int] with .asInstanceOf[Array[Int]])
      // check if it is needed to manually wrap the blocks returning type if it is of type T 
      // tree.tpe.find(_ =:= typeOf_T) might be useful.
      val expr_f_Int = c.Expr[Any](cast(subs(expr_f.tree.duplicate, typeOf[Int]), typeOf_f))
      val expr_f_Double = c.Expr[Any](cast(subs(expr_f.tree.duplicate, typeOf[Double]), typeOf_f))
      val expr_f_Boolean = c.Expr[Any](cast(subs(expr_f.tree.duplicate, typeOf[Boolean]), typeOf_f))

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      // TODO: create alternative variant selection (try with match{})
      val newExpr =
         c.Expr(cast(reify {
            if (classTag.splice == manifest[Int]) {
               println("executing Int branch")
               expr_f_Int.splice
            } else if (classTag.splice == manifest[Double]) {
               println("executing Double branch")
               expr_f_Double.splice
            } else if (classTag.splice == manifest[Boolean]) {
               println("executing Boolean branch")
               expr_f_Boolean.splice
            } else {
               println("executing Generic branch")
               expr_f.splice
            }
         }.tree, typeOf_f))

      // RETURN THE TREE
      return c.Expr(Block(List( // TODO remove this return
         printblockTree(c)("expr_f: " + showRaw(expr_f.tree)),
         printblockTree(c)(show(expr_f.tree)),
         printblockTree(c)("\n" + "newExprWithCast: " + showRaw(newExpr.tree, printTypes = true)),
         printblockTree(c)(show(newExpr.tree) + "\n")),
         //newExpr.tree))
         expr_f.tree))
      //return newExpr

   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }

}