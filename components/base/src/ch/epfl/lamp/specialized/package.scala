package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.AnyValManifest
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.collection.mutable.MapBuilder

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
      val DefDef(_, encMethName, _, encMethVparamss, _, _) = c.enclosingMethod

      val mapping = getTemsMapping(c)(expr_f, typeOf_T)

      // COMPILES SPECIFIC VARIANTS INTO ONE TREE
      val specMethNameInt = c.fresh(newTermName(f"${encMethName}_spec_Int"))
      val specMethNameDouble = c.fresh(newTermName(f"${encMethName}_spec_Double"))
      val specMethNameBoolean = c.fresh(newTermName(f"${encMethName}_spec_Boolean"))

      val specMethInt = createSpecMethod(c)(classTag, typeOf_T, typeOf[Int], expr_f, specMethNameInt, mapping)
      val specMethDouble = createSpecMethod(c)(classTag, typeOf_T, typeOf[Double], expr_f, specMethNameDouble, mapping)
      val specMethBoolean = createSpecMethod(c)(classTag, typeOf_T, typeOf[Boolean], expr_f, specMethNameBoolean, mapping)

      // val specCallers = SpecMethodMaker.createSpecCallers(c)(classTag, typeOf_T, typeOf_f, specMethName, identsMapping, selectTypeRefsMapping)

      val newExpr = reify {
         specMethInt.splice
         specMethDouble.splice
         specMethBoolean.splice
         //specCallers.splice
         expr_f.splice
      }

      // RETURN THE NEW TREE
      //      c.warning(classTag.tree.pos, "defDefsInside = " + show(defDefsInside))
      //      c.warning(classTag.tree.pos, "identsInside = " + show(identsInside))
      //      c.warning(classTag.tree.pos, "selectTypeRefsInside = " + show(selectTypeRefsInside))
      //      c.warning(classTag.tree.pos, "valDefsIside = " + show(valDefsIside))
      c.warning(classTag.tree.pos, "newExpr = " + show(newExpr))
      newExpr // newExpr
   }

   private def getTemsMapping(c: Context)(expr: c.Expr[Any], typeOf_T: c.Type) = {
      import c.universe._

      val fieldsMapping = scala.collection.mutable.Map.empty[String, (Name, Type)]
      val valdefInside = scala.collection.mutable.Set.empty[String]

      object traverser extends Traverser {
         override def traverse(tree: Tree) = tree match {
            case select @ Select(term, name) if select.tpe != null && select.tpe.widen.exists(_ == typeOf_T) =>
               select.tpe.widen match {
                  case _: TypeRef =>
                     val strRep = select.toString
                     if (!fieldsMapping.contains(strRep))
                        fieldsMapping += (strRep -> (c.fresh(newTermName(strRep.toString.replace(".", "_"))), select.tpe.widen))
                     super.traverse(tree)
                  case _ => super.traverse(tree)
               }
            case valDef @ ValDef(mod, name, tpt, rhs) =>
               valdefInside += name.toString
               super.traverse(tree)
            case ident @ Ident(name) if ident.tpe != null && ident.tpe.widen.exists(_ == typeOf_T) =>
               val strRep = name.toString
               if (!fieldsMapping.contains(strRep))
                  fieldsMapping += (strRep -> (newTermName(strRep), ident.tpe.widen))
            case _ => super.traverse(tree)
         }
      }

      traverser.traverse(expr.tree)

      fieldsMapping -- valdefInside

      fieldsMapping.toMap
   }

   private def createSpecMethod[T](c: Context)(classTag: c.Expr[ClassTag[T]], typeOf_T: c.Type, typeOf_Spec: c.Type, body: c.Expr[Any], specMethName: c.universe.TermName, mapping: Map[String, (c.Name, c.Type)]): c.Expr[Any] = {
      import c.universe._

      def specializedBody(tree: Tree): Tree = tree match {
         case block @ Block(trees, last)          => Block(trees map (specializedBody(_)), specializedBody(last))
         case apply @ Apply(func, params)         => Apply(specializedBody(func), params map (specializedBody(_)))
         case typeApply @ TypeApply(func, params) => TypeApply(specializedBody(func), params map (specializedBody(_)))
         case select @ Select(term, name) =>
            mapping.get(select.toString) match {
               case Some((newName, _)) => Ident(newName)
               case None               => Select(specializedBody(term), name)
            }
         case select @ Select(term, name)          => Select(specializedBody(term), name)
         case typeTree: TypeTree                   => typeTree
         case valDef @ ValDef(mod, name, tpt, rhs) => ValDef(mod, name, tpt, specializedBody(rhs))
         case lit: Literal                         => lit
         case ths: This                            => ths
         case Ident(name) =>
            mapping.get(name.toString) match {
               case Some((newName, _)) => Ident(newName)
               case None               => Ident(name)
            }
         case mtch @ Match(param, classes) => mtch
         case x                            => tree
      }

      val Block(DefDef(mods, _, tparams, _, tpt, rhs) :: Nil, _) = reify { def spec() = {} }.tree

      val newBody = specializedBody(body.tree).substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec)) // specializedBody(body.tree)

      val vparamss = List(
         (for (field <- mapping.keys) yield {
            ValDef(
               Modifiers(Flag.PARAM), newTermName(mapping(field)._1.toString),
               TypeTree().setType(mapping(field)._2.substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec))),
               EmptyTree)
         }).toList)

      c.Expr[Any](DefDef(mods, specMethName, tparams, vparamss, TypeTree(), c.resetAllAttrs(newBody)))
   }

   private def subsType(c: Context)(tree: c.Tree, from: c.Type, to: c.Type): c.universe.TypeTree = {
      val typeTree = c.universe.TypeTree()
      typeTree.setType(tree.tpe.widen.substituteTypes(List(from.typeSymbol), List(to)))
      typeTree
   }
}