package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.AnyValManifest
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.collection.mutable.MapBuilder
import scala.AnyValCompanion
import scala.reflect.internal.Flags

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

      c.resetAllAttrs(expr_f.tree)

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

      // RETRIEVE DEVINITIONS AND USES OF TERMS THAT HAVE T IN THE TYPE
      val DefDef(_, encMethName, _, _, _, _) = c.enclosingMethod
      val mapping = getTemsMapping(c)(expr_f, typeOf_T)

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      val specMethodNameGeneric = c.fresh(newTermName(f"${encMethName}_spec_Generic"))
      val specMethodNamesAndTypes: Map[Object, (TermName, Type)] = Map(
         Int -> (c.fresh(newTermName(f"${encMethName}_spec_Int")), typeOf[Int]),
         Double -> (c.fresh(newTermName(f"${encMethName}_spec_Double")), typeOf[Double]),
         Boolean -> (c.fresh(newTermName(f"${encMethName}_spec_Boolean")), typeOf[Boolean]))

      val specMethodInt = createSpecializedMethod(c)(typeOf_T, expr_f, specMethodNamesAndTypes(Int), mapping)
      val specMethodDouble = createSpecializedMethod(c)(typeOf_T, expr_f, specMethodNamesAndTypes(Double), mapping)
      val specMethodBoolean = createSpecializedMethod(c)(typeOf_T, expr_f, specMethodNamesAndTypes(Boolean), mapping)
      val specMethodGeneric = createGenericMethod(c)(specMethodNameGeneric, expr_f)

      val specCallers = createSpecCallers(c)(classTag, typeOf_T, typeOf_f, mapping, specMethodNamesAndTypes, specMethodNameGeneric)

      val newExpr = reify {
         specMethodInt.splice
         specMethodDouble.splice
         specMethodBoolean.splice
         specMethodGeneric.splice
         specCallers.splice
      }

      // RETURN THE NEW TREE
//      c.warning(classTag.tree.pos, "newExpr = " + show(newExpr))
      newExpr // newExpr
   }

   private def getTemsMapping[T](c: Context)(expr: c.Expr[Any], typeOf_T: c.Type): Map[String, (String, c.Type, c.Tree)] = {
      import c.universe._

      val fieldsMapping = scala.collection.mutable.Map.empty[String, (String, Type, Tree)]
      val valdefInside = scala.collection.mutable.Set.empty[String]
      val defdefInside = scala.collection.mutable.Set.empty[String]

      object traverser extends Traverser {
         override def traverse(tree: Tree) = tree match {
            case select @ Select(term, name) if select.tpe != null && select.tpe.widen.exists(_ == typeOf_T) =>
               select.tpe.widen match {
                  case _: TypeRef =>
                     val strRep = select.toString
                     if (!fieldsMapping.contains(strRep))
                        fieldsMapping += (strRep -> (c.fresh(newTermName(strRep.toString.replace(".", "_"))).toString, select.tpe.widen, select))
                     super.traverse(tree)
                  case _ => super.traverse(tree)
               }
            case valDef @ ValDef(mod, name, tpt, rhs) =>
               valdefInside += name.toString
               super.traverse(tree)
            case ident @ Ident(name) if ident.tpe != null && ident.tpe.widen.exists(_ == typeOf_T) =>
               val strRep = name.toString
               if (!fieldsMapping.contains(strRep))
                  fieldsMapping += (strRep -> (strRep, ident.tpe.widen, ident))
            case defdef @ DefDef(mods, name, tparams, _, tpt, _) if tpt.tpe != null && tpt.tpe.widen.exists(_ == typeOf_T) =>
               defdefInside += name.toString
               super.traverse(tree)
            case _ => super.traverse(tree)
         }
      }

      traverser.traverse(expr.tree)

      fieldsMapping --= valdefInside
      fieldsMapping --= defdefInside

      fieldsMapping.toMap
   }

   private def createSpecializedMethod[T](c: Context)(typeOf_T: c.Type, body: c.Expr[Any], methodNameAndType: (c.TermName, c.Type), mapping: Map[String, (String, c.Type, c.Tree)]): c.Expr[Any] = {
      import c.universe._
      val (methodName, typeOf_Spec) = methodNameAndType

      def specializedBody(tree: Tree): Tree = tree match {
         case select @ Select(term, name) =>
            mapping.get(select.toString) match {
               case Some((newName, _, _)) => Ident(newName)
               case None                  => Select(specializedBody(term), name)
            }
         case Ident(name) =>
            mapping.get(name.toString) match {
               case Some((newName, _, _)) => Ident(newName)
               case None                  => Ident(name)
            }
         case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            val newVparamss = vparamss map (_ map { case ValDef(mods, name, tpt, rhs) => ValDef(mods, name, tranformTypeTree(c)(tpt, typeOf_T, typeOf_Spec), specializedBody(rhs)) })
            DefDef(mods, name, tparams, newVparamss, tranformTypeTree(c)(tpt, typeOf_T, typeOf_Spec), specializedBody(rhs))
         case Function(vparams, body) =>
            val newVparams = vparams map { case ValDef(mods, name, tpt, rhs) => ValDef(mods, name, tranformTypeTree(c)(tpt, typeOf_T, typeOf_Spec), specializedBody(rhs)) }
            Function(newVparams, specializedBody(body))
         case If(cond, thenp, elsep)      => If(specializedBody(cond), specializedBody(thenp), specializedBody(elsep))
         case Block(trees, last)          => Block(trees map (specializedBody(_)), specializedBody(last))
         case Apply(func, params)         => Apply(specializedBody(func), params map (specializedBody(_)))
         case TypeApply(func, params)     => TypeApply(specializedBody(func), params map (specializedBody(_)))
         case Select(term, name)          => Select(specializedBody(term), name)
         case ValDef(mod, name, tpt, rhs) => ValDef(mod, name, tpt, specializedBody(rhs))
         case typeTree: TypeTree          => typeTree
         case lit: Literal                => lit
         case ths: This                   => ths
         case EmptyTree                   => EmptyTree

         case _                           => c.warning(body.tree.pos, "TODO: add case in createSpecializedMethod: " + showRaw(tree)); tree
      }

      val Block(DefDef(mods, _, tparams, _, tpt, rhs) :: Nil, _) = reify { def spec() = {} }.tree

      val newBody = specializedBody(body.tree).substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec))

      val vparamss = List(
         (for (field <- mapping.keys) yield {
            ValDef(
               Modifiers(Flag.PARAM), newTermName(mapping(field)._1),
               TypeTree().setType(mapping(field)._2.substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec))),
               EmptyTree)
         }).toList)

      c.Expr[Any](DefDef(mods, methodName, tparams, vparamss, TypeTree(), c.resetAllAttrs(newBody)))
   }

   private def createGenericMethod[T](c: Context)(methodName: c.universe.TermName, body: c.Expr[Any]): c.Expr[Any] = {
      import c.universe._
      c.Expr[Any](DefDef(Modifiers(), methodName, List(), List(List()), TypeTree(), c.resetAllAttrs(body.tree)))
   }

   private def createSpecCallers[T](c: Context)(classTag: c.Expr[ClassTag[T]], typeOf_T: c.Type, typeOf_f: c.Type, mapping: Map[String, (String, c.Type, c.Tree)], specMethodNamesAndTypes: Map[Object, (c.TermName, c.Type)], specMethodNameGeneric: c.Name) = {
      import c.universe._

      def createSpecCaller(nameAndType: (Name, Type)) = {
         val (name, tpe) = nameAndType
         c.Expr(
            Apply(
               Ident(name),
               (for (key <- mapping.keys) yield {
                  castTree(c)(
                     c.resetAllAttrs(mapping(key)._3),
                     TypeTree().setType(mapping(key)._2).substituteTypes(List(typeOf_T.typeSymbol), List(tpe)).tpe)
               }).toList))
      }

      val callIntSpec = createSpecCaller(specMethodNamesAndTypes(Int))
      val callDoubleSpec = createSpecCaller(specMethodNamesAndTypes(Double))
      val callBooleanSpec = createSpecCaller(specMethodNamesAndTypes(Boolean))
      val callGenSpec = c.Expr(Apply(Ident(specMethodNameGeneric), Nil))

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

   private def tranformTypeTree(c: Context)(typeTree: c.Tree, typeOf_T: c.Type, typeOf_Spec: c.Type): c.Tree = {
      import c.universe._
      TypeTree().setType(typeTree.tpe.widen.substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec)))
   }

}