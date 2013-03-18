package ch.epfl.lamp.specialized

import scala.reflect.macros.Context

object SpecTraversers {

   def getDefDefsWithTypeParam(c: Context)(expr: c.Expr[Any], typeOf_TypeParam: c.Type): Set[c.universe.DefDef] = getTermsWithTypeParam(c)(expr, typeOf_TypeParam)._1.toSet
   def getIdentsWithTypeParam(c: Context)(expr: c.Expr[Any], typeOf_TypeParam: c.Type): Set[c.universe.Ident] = getTermsWithTypeParam(c)(expr, typeOf_TypeParam)._2.toSet
   def getSelectTypeRefWithTypeParam(c: Context)(expr: c.Expr[Any], typeOf_TypeParam: c.Type): Set[c.universe.Select] = getTermsWithTypeParam(c)(expr, typeOf_TypeParam)._3.toSet
   def getValDefsWithTypeParam(c: Context)(expr: c.Expr[Any], typeOf_TypeParam: c.Type): Set[c.universe.ValDef] = getTermsWithTypeParam(c)(expr, typeOf_TypeParam)._4.toSet

   private def getTermsWithTypeParam(c: Context)(expr: c.Expr[Any], typeOf_TypeParam: c.Type) = {
      import c.universe._
      @inline def checkTpe(tree: Tree): Boolean = if (tree.tpe == null) false else tree.tpe.widen.exists(_ == typeOf_TypeParam)
      object traverser extends Traverser {
         var defdefs = scala.collection.mutable.Set.empty[DefDef]
         var idents = scala.collection.mutable.Set.empty[Ident]
         var selectTypeRefs = scala.collection.mutable.Set.empty[Select]
         var valdefs = scala.collection.mutable.Set.empty[ValDef]
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
      // Find definition and uses inside expr
      traverser.traverse(expr.tree)

      (traverser.defdefs, traverser.idents, traverser.selectTypeRefs, traverser.valdefs)
   }

   //   @inline def checkTpe(tree: Tree): Boolean = if (tree.tpe == null) false else tree.tpe.widen.exists(_ == typeOf_T)
   //   abstract class specializedTraverser extends Traverser {
   //      var defdefs = scala.collection.mutable.Set.empty[DefDef]
   //      var idents = scala.collection.mutable.Set.empty[Ident]
   //      var selectTypeRefs = scala.collection.mutable.Set.empty[Select]
   //      var valdefs = scala.collection.mutable.Set.empty[ValDef]
   //
   //      // Debug
   //      def defdefNames: Set[Name] = defdefs.toSet[DefDef] map { case DefDef(_, name, _, _, _, _) => name }
   //      def identNames: Set[Name] = idents.toSet[Ident] map { case Ident(name) => name }
   //      def selectTypeRefNames: Set[Name] = selectTypeRefs.toSet[Select] map { case Select(_, name) => name }
   //      def valdefNames: Set[Name] = valdefs.toSet[ValDef] map { case ValDef(_, name, _, _) => name }
   //   }
   //
   //   object traverser_f extends specializedTraverser {
   //      override def traverse(tree: Tree): Unit = {
   //         tree match {
   //            case defdef @ DefDef(_, _, _, _, tpt, _) if checkTpe(tpt) => defdefs += defdef
   //            case ident: Ident if checkTpe(ident)                      => idents += ident
   //            case select: Select if checkTpe(select) =>
   //               select.tpe.widen match {
   //                  case _: MethodType =>
   //                  case _: TypeRef    => selectTypeRefs += select
   //                  case _             =>
   //               }
   //            case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) => valdefs += valdef
   //            case _ =>
   //         }
   //         super.traverse(tree)
   //      }
   //   }
   //
   //   object traverser_enclosingMethod extends specializedTraverser {
   //      override def traverse(tree: Tree): Unit = {
   //         tree match {
   //            case apply @ Apply(fun: TypeApply, args) =>
   //            // c.warning(classTag.tree.pos, ">>> " + showRaw(apply))
   //            // TODO identify Apply(TypeApply(Ident(newTermName("specialized")), List(Ident(newTypeName("T")))), ...)
   //            // and only ignore contents (i.e don't call super.traverse(tree))
   //            case defdef @ DefDef(_, _, _, _, tpt, _) if checkTpe(tpt) =>
   //               defdefs += defdef
   //               super.traverse(tree)
   //            case ident: Ident if checkTpe(ident) =>
   //               idents += ident
   //               super.traverse(tree)
   //            case select: Select if checkTpe(select) =>
   //               select.tpe.widen match {
   //                  case _: MethodType =>
   //                  case _: TypeRef    => selectTypeRefs += select
   //                  case _             =>
   //               }
   //               super.traverse(tree)
   //            case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) =>
   //               valdefs += valdef
   //               super.traverse(tree)
   //            case _ => super.traverse(tree)
   //         }
   //      }
   //   }
   //
   //   object traverser_enclosingClass extends specializedTraverser {
   //      override def traverse(tree: Tree): Unit = {
   //         tree match {
   //            case defdef @ DefDef(_, _, _, _, tpt, _) if checkTpe(tpt) => defdefs += defdef
   //            case ident: Ident if checkTpe(ident)                      => idents += ident
   //            case select: Select if checkTpe(select) =>
   //               select.tpe.widen match {
   //                  case _: MethodType =>
   //                  case _: TypeRef    => selectTypeRefs += select
   //                  case _             =>
   //               }
   //            case valdef @ ValDef(_, _, tpt, _) if checkTpe(tpt) => valdefs += valdef
   //            case _ =>
   //         }
   //      }
   //   }
   //
   //   // Find definition and uses inside the specialized block
   //   traverser_f.traverse(expr_f.tree)
   //
   //   // Find definition and uses inside the enclosing function and outside the specialized
   //   //      traverser_enclosingMethod.traverse(c.enclosingMethod match { case DefDef(_, _, _, _, _, rhs) => rhs })
   //   //      traverser_enclosingMethod.valdefs ++= (c.enclosingMethod match { case DefDef(_, _, _, vparamss, _, _) => for (vparams <- vparamss; vparam <- vparams) yield vparam; case _ => Set.empty[ValDef] })
   //
   //   // Find definition and uses inside the enclosing class
   //   //      c.enclosingClass match { case ClassDef(_, _, _, Template(_, _, body)) => for (tree <- body) traverser_enclosingClass.traverse(tree) }
   //
   //   val traverser_debug = traverser_f
   //   // Debug
   //   //      c.warning(classTag.tree.pos, "defdefs ::>>> " + traverser_debug.defdefs.map(showRaw(_)))
   //   //      c.warning(classTag.tree.pos, "idents ::>>> " + traverser_debug.idents.map(showRaw(_)))
   //   if (debug) c.warning(classTag.tree.pos, "selectsTypeRef ::>>> " + traverser_debug.selectTypeRefs.map(showRaw(_)))
   //   //      c.warning(classTag.tree.pos, "valdefs ::>>> " + traverser_debug.valdefs.map(showRaw(_)))

}