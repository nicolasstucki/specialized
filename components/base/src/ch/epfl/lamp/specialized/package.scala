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

      // IDENTIFY VALs, VARs, DEFs DEFINITIONS AND USES IN BLOCK 
      @inline def checkTpe(tree: Tree): Boolean = if (tree.tpe == null) false else tree.tpe.widen.exists(_ == typeOf_T)
      abstract class specializedTraverser extends c.universe.Traverser {
         var defdefs = scala.collection.mutable.Set.empty[DefDef]
         var idents = scala.collection.mutable.Set.empty[Ident]
         var selectTypeRefs = scala.collection.mutable.Set.empty[Select]
         var valdefs = scala.collection.mutable.Set.empty[ValDef]

         // Debug
         def defdefNames: Set[Name] = defdefs.toSet[DefDef] map { case DefDef(_, name, _, _, _, _) => name }
         def identNames: Set[Name] = idents.toSet[Ident] map { case Ident(name) => name }
         def selectTypeRefNames: Set[Name] = selectTypeRefs.toSet[Select] map { case Select(_, name) => name }
         def valdefNames: Set[Name] = valdefs.toSet[ValDef] map { case ValDef(_, name, _, _) => name }
      }

      object traverser_f extends specializedTraverser {
         override def traverse(tree: Tree): Unit = {
            tree match {
               case defdef @ DefDef(_, _, _, _, tpt, _) if checkTpe(tpt) => defdefs += defdef
               case ident: Ident if checkTpe(ident)                      => idents += ident
               case select: Select if checkTpe(select) =>
                  select.tpe.widen match {
                     case _: MethodType =>
                     case _: TypeRef    => selectTypeRefs += select
                     case _             =>
                  }
               case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) => valdefs += valdef
               case _ =>
            }
            super.traverse(tree)
         }
      }

      object traverser_enclosingMethod extends specializedTraverser {
         override def traverse(tree: Tree): Unit = {
            tree match {
               case apply @ Apply(fun: TypeApply, args) =>
               // c.warning(classTag.tree.pos, ">>> " + showRaw(apply))
               // TODO identify Apply(TypeApply(Ident(newTermName("specialized")), List(Ident(newTypeName("T")))), ...)
               // and only ignore contents (i.e don't call super.traverse(tree))
               case defdef @ DefDef(_, _, _, _, tpt, _) if checkTpe(tpt) =>
                  defdefs += defdef
                  super.traverse(tree)
               case ident: Ident if checkTpe(ident) =>
                  idents += ident
                  super.traverse(tree)
               case select: Select if checkTpe(select) =>
                  select.tpe.widen match {
                     case _: MethodType =>
                     case _: TypeRef    => selectTypeRefs += select
                     case _             =>
                  }
                  super.traverse(tree)
               case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) =>
                  valdefs += valdef
                  super.traverse(tree)
               case _ => super.traverse(tree)
            }
         }
      }

      object traverser_enclosingClass extends specializedTraverser {
         override def traverse(tree: Tree): Unit = {
            tree match {
               case defdef @ DefDef(_, _, _, _, tpt, _) if checkTpe(tpt) => defdefs += defdef
               case ident: Ident if checkTpe(ident)                      => idents += ident
               case select: Select if checkTpe(select) =>
                  select.tpe.widen match {
                     case _: MethodType =>
                     case _: TypeRef    => selectTypeRefs += select
                     case _             =>
                  }
               case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) => valdefs += valdef
               case _ =>
            }
         }
      }

      // Find definition and uses inside the specialized block
      //      traverser_f.traverse(expr_f.tree)

      // Find definition and uses inside the enclosing function and outside the specialized
      //      traverser_enclosingMethod.traverse(c.enclosingMethod match { case DefDef(_, _, _, _, _, rhs) => rhs })
      //      traverser_enclosingMethod.valdefs ++= (c.enclosingMethod match { case DefDef(_, _, _, vparamss, _, _) => for (vparams <- vparamss; vparam <- vparams) yield vparam; case _ => Set.empty[ValDef] })

      // Find definition and uses inside the enclosing class
      //      c.enclosingClass match { case ClassDef(_, _, _, Template(_, _, body)) => for (tree <- body) traverser_enclosingClass.traverse(tree) }

      //      val traverser_debug = traverser_enclosingClass
      // Debug
      //      c.warning(classTag.tree.pos, "defdefs ::>>> " + traverser_debug.defdefs.map(showRaw(_)))
      //      c.warning(classTag.tree.pos, "idents ::>>> " + traverser_debug.idents.map(showRaw(_)))
      //      c.warning(classTag.tree.pos, "selectsTypeRef ::>>> " + traverser_debug.selectTypeRefs.map(showRaw(_)))
      //      c.warning(classTag.tree.pos, "valdefs ::>>> " + traverser_debug.valdefs.map(showRaw(_)))

      // Debug
      //      c.warning(classTag.tree.pos, "defdefNames ::>>> " + traverser_debug.defdefNames.map(showRaw(_)))
      //      c.warning(classTag.tree.pos, "identNames ::>>> " + traverser_debug.identNames.map(showRaw(_)))
      //      c.warning(classTag.tree.pos, "selectsTypeRefName ::>>> " + traverser_debug.selectTypeRefNames.map(showRaw(_)))
      //      c.warning(classTag.tree.pos, "valdefNames ::>>> " + traverser_debug.valdefNames.map(showRaw(_)))

      // CREATE SPECIALIZED METHOD
//      val vparams = List.empty[ValDef]
//      val enclMethod = c.enclosingMethod
//      val ddef = build.newNestedSymbol(enclMethod.symbol, newTermName(c.fresh("spec")), enclMethod.pos, /*Flag.PRIVATE |*/ Flag.DEFERRED /*| METHOD*/, isClass = false)
//      val params = vparams map { vd =>
//         val sym = build.newNestedSymbol(ddef, vd.name, ddef.pos, Flag.PARAM, isClass = false)
//         build.setTypeSignature(sym, vd.tpt.tpe)
//         vd setSymbol sym
//         sym
//      }
//      build.setTypeSignature(ddef, MethodType(params, typeOf_f)).asMethod

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

      val expr_f_Int = c.Expr[Any](subs(expr_f.tree.duplicate, typeOf[Int]))
      val expr_f_Double = c.Expr[Any](subs(expr_f.tree.duplicate, typeOf[Double]))
      val expr_f_Boolean = c.Expr[Any](subs(expr_f.tree.duplicate, typeOf[Boolean]))

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      // TODO: create alternative variant selection (try with match{})
      val newExpr =
         c.Expr(cast(reify {
            if (classTag.splice == manifest[Int]) {
               expr_f_Int.splice
            } else if (classTag.splice == manifest[Double]) {
               expr_f_Double.splice
            } else if (classTag.splice == manifest[Boolean]) {
               expr_f_Boolean.splice
            } else {
               expr_f.splice
            }
         }.tree, typeOf_f))

      // RETURN THE NEW TREE
      c.warning(classTag.tree.pos, show(newExpr))
      newExpr
   }

   private def printblockTree(c: Context)(str: String) = {
      import c.universe._
      Apply(Ident(newTermName("println")), List(Literal(Constant(str))))
   }

}