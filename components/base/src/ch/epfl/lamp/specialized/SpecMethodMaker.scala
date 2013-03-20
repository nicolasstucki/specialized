package ch.epfl.lamp.specialized

import scala.reflect.ClassTag
import scala.reflect.macros.Context

object SpecMethodMaker {

   def createSpecMethod[T](c: Context)(classTag: c.Expr[ClassTag[T]], typeOf_T: c.Type, body: c.Expr[Any], specMethName: c.universe.TermName, identsMapping: Map[String, (c.universe.Name, c.universe.Ident)], selectTypeRefsMapping: Map[String, (c.Name, c.universe.Select)]): c.Expr[Any] = {
      import c.universe._

      val Block(DefDef(mods, _, TypeDef(tdModsX, tdName, tdTparamsX, tdRhsX) :: Nil, _, _, _) :: Nil, _) = reify { def spec[@specialized U]() = {} }.tree

      val newTypeParamName = tdName // newTypeName(c.fresh("U"))
      val typeOf_Spec = typeOf[Any] // TODO add correct type ( typeOf[U] )

      var valDefs = Set.empty[String]
      def specBody(tree: Tree): Tree = {
         tree match {
            case block @ Block(trees, last)          => Block(trees map (specBody(_)), specBody(last))
            case apply @ Apply(func, params)         => Apply(specBody(func), params map (specBody(_)))
            case typeApply @ TypeApply(func, params) => TypeApply(specBody(func), params map (specBody(_)))
            case select @ Select(term, name) =>
               (selectTypeRefsMapping -- valDefs).get(select.toString) match {
                  case Some((newName, _)) => Ident(newName)
                  case None               => Select(specBody(term), name)
               }
            case typeTree: TypeTree => typeTree
            case valDef @ ValDef(mod, name, tpt, rhs) => {
               val newType = subsType(c)(tpt, typeOf_T, typeOf_Spec)
               valDefs = valDefs + name.toString
               ValDef(mod, name, newType, specBody(rhs))
            }
            case lit: Literal => lit
            case ths: This    => ths
            case ident @ Ident(name) => Ident(name)/*
               identsMapping.get(ident.toString) match {
                  case Some((newName, _)) => Ident(newName)
                  case None               => Ident(ident.name)
               }*/
            case mtch @ Match(param, classes) => mtch
            case x                            => tree
         }
      }

      val newBody = c.Expr[Any](specBody(body.tree))

      val vparamss = {
         List(
            (for (key <- selectTypeRefsMapping.keys) yield {
               ValDef(
                  Modifiers(Flag.PARAM), newTermName(selectTypeRefsMapping(key)._1.toString),
                  AppliedTypeTree(Ident(newTypeName("Array")), List(Ident(newTypeParamName))), // TODO remove this when typeOf_Spec works
                  // TypeTree().setType(subsType(c)(selectTypeRefsMapping(key)._2, typeOf_T, typeOf_Spec).tpe),
                  EmptyTree)
            }).toList /*::: (for (key <- identsMapping.keys) yield {
               ValDef(
                  Modifiers(Flag.PARAM), newTermName(identsMapping(key)._1.toString),
                  AppliedTypeTree(Ident(newTypeName("Array")), List(Ident(newTypeParamName))), // TODO remove this when typeOf_Spec works
                  // TypeTree().setType(subsType(c)(identsMapping(key)._2, typeOf_T, typeOf_Spec).tpe),
                  EmptyTree)
            }).toList*/)
      }

      //c.warning(classTag.tree.pos, "typeOf_Spec = " + show(typeOf_Spec))

      c.Expr[Any](DefDef(mods, specMethName, List(TypeDef(tdModsX, newTypeParamName, tdTparamsX, tdRhsX)), vparamss, TypeTree(), newBody.tree))
   }

   def createSpecCallers[T](c: Context)(classTag: c.Expr[ClassTag[T]], typeOf_T: c.Type, typeOf_f: c.Type, specMethName: c.universe.TermName, identsMapping: Map[String, (c.universe.Name, c.universe.Ident)], selectTypeRefsMapping: Map[String, (c.Name, c.universe.Select)]) = {
      import c.universe._

      def createSpecCaller(tpeOpt: Option[Type]) = {
         c.Expr(
            Apply(
               TypeApply(
                  Ident(specMethName),
                  tpeOpt match {
                     case Some(tpe) => List(TypeTree().setType(tpe));
                     case None      => List(TypeTree().setType(typeOf_T))
                  }),
               (for (key <- selectTypeRefsMapping.keys) yield {
                  tpeOpt match {
                     case Some(tpe) => castTree(c)(selectTypeRefsMapping(key)._2, subsType(c)(selectTypeRefsMapping(key)._2, typeOf_T, tpe).tpe)
                     case None      => selectTypeRefsMapping(key)._2
                  }
               }).toList /* ::: (for (key <- identsMapping.keys) yield {
                  tpeOpt match {
                     case Some(tpe) => castTree(c)(identsMapping(key)._2, subsType(c)(identsMapping(key)._2, typeOf_T, tpe).tpe)
                     case None      => identsMapping(key)._2
                  }
               }).toList */ ))
      }

      val callIntSpec = createSpecCaller(Some(typeOf[Int]))
      val callDoubleSpec = createSpecCaller(Some(typeOf[Double]))
      val callBooleanSpec = createSpecCaller(Some(typeOf[Boolean]))
      val callGenSpec = createSpecCaller(None)

      val callersBlock = reify {
         if (classTag.splice == manifest[Int]) {
            callIntSpec.splice
         } else if (classTag.splice == manifest[Double]) {
            callDoubleSpec.splice
         } else if (classTag.splice == manifest[Boolean]) {
            callBooleanSpec.splice
         } else {
            callGenSpec.splice
         }
      }

      castExpr(c)(callersBlock, typeOf_f)
   }

   private def castExpr(c: Context)(expr: c.Expr[Any], tpe: c.Type): c.Expr[Any] = c.Expr[Any](castTree(c)(expr.tree, tpe))

   private def castTree(c: Context)(tree: c.Tree, tpe: c.Type): c.Tree = {
      import c.universe._
      TypeApply(Select(tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe)))
   }

   private def subsType(c: Context)(tree: c.Tree, from: c.Type, to: c.Type): c.universe.TypeTree = {
      val typeTree = c.universe.TypeTree()
      typeTree.setType(tree.tpe.widen.substituteTypes(List(from.typeSymbol), List(to)))
      typeTree
   }

}