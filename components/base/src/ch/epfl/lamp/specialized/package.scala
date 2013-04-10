package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.Specializable._
import scala.reflect.ManifestFactory
import scala.collection.SortedMap

/**
 * @author Nicolas Stucki
 *
 */
object `package` {

   private[this] implicit object SpecializableOrdering extends Ordering[Specializable] {
      def compare(a: Specializable, b: Specializable) = a.toString compare b.toString
   }

   /**
    * Specialized block
    * @param expr_f: Code inside the specialized block
    * @param classTag: Implicit ClassTag of the type being specialized
    * @return: Original return statement of the block
    */
   def specialized[T](expr_f: => Any)(implicit classTag: ClassTag[T]): Any = macro impl_specialized_default_types[T]

   /**
    * Specialized block
    * @param specGroup: types that will be specialized
    * @param expr_f: Code inside the specialized block
    * @param classTag: Implicit ClassTag of the type being specialized
    * @return: Original return statement of the block
    */
   def specialized[T](specGroup: SpecializedGroup)(expr_f: => Any)(implicit classTag: ClassTag[T]): Any = macro impl_specialized_group[T]

   /**
    * Specialized block
    * @param types: types that will be specialized
    * @param expr_f: Code inside the specialized block
    * @param classTag: Implicit ClassTag of the type being specialized
    * @return: Original return statement of the block
    */
   def specialized[T](types: Specializable*)(expr_f: => Any)(implicit classTag: ClassTag[T]): Any = macro impl_specialized[T]

   /**
    * Macro implementation of specialized[T]{...}
    * @param c: Context
    * @param expr_f: Code inside the specialized block
    * @param classTag: ClassTag of the type being specialized
    * @param typetagT: Implicit WeakTypeTag of the type being specialized
    * @return: Code inside the specialized block with specialization
    */
   def impl_specialized_default_types[T](c: Context)(expr_f: c.Expr[Any])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      impl_specialized_group[T](c)(c.universe.reify { Specializable.Primitives })(expr_f)(classTag)(typetagT)
   }

   /**
    * Macro implementation of specialized[T]{...}
    * @param c: Context
    * @param specGroupExpr: Types to be specialized. If empty, default specialization is used (Int,Double,Boolean)
    * @param expr_f: Code inside the specialized block
    * @param classTag: ClassTag of the type being specialized
    * @param typetagT: Implicit WeakTypeTag of the type being specialized
    * @return: Code inside the specialized block with specialization
    */
   def impl_specialized_group[T](c: Context)(specGroup: c.Expr[SpecializedGroup])(expr_f: c.Expr[Any])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      import c.universe._
      // This is a hack and should be changed
      // Note that SpecializedGroup can be passed directly to @specialized(group)
      val group = c eval c.Expr[SpecializedGroup](c resetAllAttrs specGroup.tree)
      val specList = (group match {
         case Primitives  => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char }, reify { Float }, reify { Double }, reify { Boolean }, reify { Unit })
         case Everything  => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char }, reify { Float }, reify { Double }, reify { Boolean }, reify { Unit } /*, reify { AnyRef }*/ )
         case Bits32AndUp => List(reify { Int }, reify { Long }, reify { Float }, reify { Double })
         case Integral    => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char })
         case AllNumeric  => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char }, reify { Float }, reify { Double })
         case BestOfBreed => List(reify { Int }, reify { Double }, reify { Boolean }, reify { Unit } /*, reify { AnyRef }*/ )
         case _           => Nil
      })
      impl_specialized[T](c)(specList: _*)(expr_f)(classTag)(typetagT)
   }

   /**
    * Macro implementation of specialized[T]{...}
    * @param c: Context
    * @param types: Types to be specialized. If empty, default specialization is used (Int,Double,Boolean)
    * @param expr_f: Code inside the specialized block
    * @param classTag: ClassTag of the type being specialized
    * @param typetagT: Implicit WeakTypeTag of the type being specialized
    * @return: Code inside the specialized block with specialization
    */
   def impl_specialized[T](c: Context)(types: c.Expr[Specializable]*)(expr_f: c.Expr[Any])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
      import c.universe._
      c.resetAllAttrs(expr_f.tree)

      // TYPE PARAMETER CHECKS
      val typeOf_T = typetagT.tpe
      val typeOf_f = expr_f.actualType

      val typesList = types.toList match {
         case list @ _ :: _ => list map (tpExpr => c eval c.Expr[Specializable](c resetAllAttrs tpExpr.tree))
         case Nil           => List(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)
      }

      // Check if T is a valid type parameter
      typeOf_T match {
         case TypeRef(NoPrefix, _, _) => // Do nothing, typeOf_T is OK
         case TypeRef(_, _, _) => {
            // This happens when:
            // 1. specialized {...} is used and there is no type parameter with a ClassTag in scope 
            // 2. or there is more than one type parameter have manifests and therefore the type could not be inferred.
            // 3. The type parameter is not a type parameter of the enclosing context, examples: specialized[Int] {...}, specialized[Any] {...}, specialized[Array[Int]] {...}, specialized[T] {...}, ...   
            c.error(classTag.tree.pos, "Specify type parameter using: specialized[T] {...}, T must be a type parameter of the enclosing context and it must have a ClassTag." +
               " Type patameter must be on top level, example: if you want to specialize an Array[T] use specialize[T] {...}.")
            return expr_f
         }
      }

      // RETRIEVE DEVINITIONS AND USES OF TERMS THAT HAVE T IN THE TYPE
      val (mapping, vardefs) = getTemsMapping(c)(expr_f, typeOf_T)

      // COMPILE SPECIFIC VARIANTS INTO ONE TREE
      val DefDef(_, encMethName, _, _, _, _) = c.enclosingMethod
      val specMethodNameGeneric = c.fresh(newTermName(f"${encMethName}_spec_Generic"))

      val specMethodNamesAndTypesList = {
         val typeToTypeTree = SortedMap[Specializable, Type](
            Int -> typeOf[Int],
            Long -> typeOf[Long],
            Double -> typeOf[Double],
            Float -> typeOf[Float],
            Char -> typeOf[Char],
            Short -> typeOf[Short],
            Byte -> typeOf[Byte],
            Boolean -> typeOf[Boolean],
            Unit -> typeOf[Unit])
         def typesIntoNamesAndTypeTrees(tpe: Specializable) = {
            val name = c.fresh(newTermName(f"${encMethName}_spec_${val str = tpe.toString; str.substring(str.lastIndexOf(".") + 1)}"))
            (tpe, name, typeToTypeTree(tpe))
         }
         typesList map (typesIntoNamesAndTypeTrees(_))
      }

      val specMethods = SortedMap((specMethodNamesAndTypesList map { case (tpnme, name, specType) => tpnme -> createSpecializedMethod(c)(typeOf_T, expr_f, name, specType, mapping, vardefs) }): _*)

      val specMethodGeneric = createGenericMethod(c)(specMethodNameGeneric, expr_f)

      val specCallers = createSpecCallers(c)(classTag, typeOf_T, typeOf_f, mapping, specMethodNamesAndTypesList, specMethodNameGeneric)

      val newExpr = c.Expr(Block(specMethodGeneric :: specMethods.values.toList, specCallers))

      // RETURN THE NEW TREE
//            c.warning(classTag.tree.pos, "newExpr = " + show(newExpr))
      newExpr
   }

   /**
    * Mapping that contains all information needed about the all the arguments that will be needed for the specialized methods.
    * The keys of the mapping contain the original names of the argument. Each is mapped into a 3-tuple that contains
    * the new name (or the same as the key if renaming is unnecesary), the widen type of the argument and the reference to the argument itself.
    * @param c: context of the macro
    * @param expr: the body of the specialized expression.
    * @param typeOf_T: the type being specialized.
    * @return the mapping of all arguments needed for specialization
    */
   @inline private def getTemsMapping[T](c: Context)(expr: c.Expr[Any], typeOf_T: c.Type): (SortedMap[String, (String, c.Type, c.Tree)], SortedMap[String, c.Type]) = {
      import c.universe._

      val fieldsMapping = scala.collection.mutable.Map.empty[String, (String, Type, Tree)]
      val valdefInside = scala.collection.mutable.Set.empty[String]
      val vardefs = scala.collection.mutable.Map.empty[String, Type]
      val defdefInside = scala.collection.mutable.Set.empty[String]

      def typeHasT(tree: Tree): Boolean = tree.tpe != null && tree.tpe.widen.exists(_ == typeOf_T)

      object bodyTraverser extends Traverser {
         var valDefsInScope = Set.empty[String]

         override def traverse(tree: Tree) = {

            val oldValDefsInScope = valDefsInScope

            tree match {
               case select @ Select(This(termName), name) if typeHasT(select) =>
                  select.tpe.widen match {
                     case _: TypeRef =>
                        val strRep = select.toString
                        if (!fieldsMapping.contains(strRep))
                           fieldsMapping += (strRep -> (c.fresh(newTermName(strRep.toString.replace(".", "_"))).toString, select.tpe.widen, select))
                     case _ =>
                  }
               case select @ Select(Ident(termName), name) if typeHasT(select) && !valDefsInScope(termName.toString) =>
                  select.tpe.widen match {
                     case _: TypeRef =>
                        val strRep = select.toString
                        if (!fieldsMapping.contains(strRep))
                           fieldsMapping += (strRep -> (c.fresh(newTermName(strRep)).toString, select.tpe.widen, select))
                     case _ =>
                  }
               case valDef @ ValDef(mod, name, tpt, rhs) =>
                  valdefInside += name.toString
                  valDefsInScope = valDefsInScope + name.toString
               case ident @ Ident(name) if typeHasT(ident) =>
                  val strRep = name.toString
                  if (!fieldsMapping.contains(strRep))
                     fieldsMapping += (strRep -> (strRep, ident.tpe.widen, ident))
               case defdef @ DefDef(mods, name, tparams, vparamss, tpt, _) if typeHasT(tpt) =>
                  defdefInside += name.toString
                  valDefsInScope = valDefsInScope ++ (vparamss flatMap (_ map { case ValDef(_, name, _, _) => name.toString }))
               case _ =>
            }

            super.traverse(tree)

            valDefsInScope = oldValDefsInScope
         }
      }

      object enclosingMethodTraverser extends Traverser {
         override def traverse(tree: Tree) = {
            tree match {
               case valDef @ ValDef(mod, name, tpt, rhs) if mod.hasFlag(Flag.MUTABLE) && !valdefInside(name.toString) => vardefs += (name.toString -> tpt.duplicate.tpe.widen)
               case _ => showRaw(tree)
            }

            super.traverse(tree)
         }
      }

      bodyTraverser.traverse(expr.tree)

      fieldsMapping --= valdefInside
      fieldsMapping --= defdefInside

      enclosingMethodTraverser.traverse(c.enclosingMethod)
      val ClassDef(_, _, _, Template(parents, self, body)) = c.enclosingClass
      val vardefsInClass = body collect { case ValDef(mods, name, _, _) if mods.hasFlag(Flag.MUTABLE) => name.toString }

      fieldsMapping --= vardefs.keys
      fieldsMapping --= vardefsInClass

      val immutableFieldsMapping = SortedMap[String, (String, c.Type, c.Tree)](fieldsMapping.toList: _*)
      val immutableVardefs = SortedMap[String, Type](vardefs.toList: _*)

      (immutableFieldsMapping, immutableVardefs)
   }

   private def createSpecializedMethod[T](c: Context)(typeOf_T: c.Type, body: c.Expr[Any], methodName: c.TermName, typeOf_Spec: c.Type, mapping: SortedMap[String, (String, c.Type, c.Tree)], vardefs: SortedMap[String, c.Type]): c.Tree = {
      import c.universe._

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
         //         case Match(selector, cases) =>
         //            val specializedCases = cases map { case CaseDef(pat, guard, body) => CaseDef(specializedBody(pat), specializedBody(guard), specializedBody(body)) }
         //            Match(specializedBody(selector), specializedCases)
         //         case Assign(Ident(name), rhs) if vardefs.isDefinedAt(name.toString) => Assign(specializedBody(Ident(name)), castTree(c)(specializedBody(rhs), vardefs(name.toString)))
         case Assign(lhs, rhs)            => Assign(specializedBody(lhs), specializedBody(rhs))
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

         case _                           => c.warning(body.tree.pos, "TODO: add case in createSpecializedMethod to handle: " + showRaw(tree)); tree
      }

      val Block(DefDef(mods, _, tparams, _, _, _) :: Nil, _) = reify { def spec() = {} }.tree

      val newBody = specializedBody(body.tree).substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec))

      val vparamss = List(
         (for (field <- mapping.keys) yield {
            ValDef(
               Modifiers(Flag.PARAM), newTermName(mapping(field)._1),
               TypeTree().setType(mapping(field)._2.substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec))),
               EmptyTree)
         }).toList)

      DefDef(mods, methodName, tparams, vparamss, TypeTree(), c.resetAllAttrs(newBody))
   }

   /**
    * Creates the method that will be called in case that the type parameter is not specialized.
    * Note that no parameters are defined because everything can be retrieved from the environment.
    * @param c: context of the macro
    * @param methodName: name of the method
    * @param body: body of the method (same as body of the specialized block)
    * @return a function definition wrapper of the generic version of the code
    */
   @inline private def createGenericMethod[T](c: Context)(methodName: c.universe.TermName, body: c.Expr[Any]): c.Tree = {
      import c.universe._
      DefDef(Modifiers(), methodName, List(), List(List()), TypeTree(), c.resetAllAttrs(body.tree))
   }

   /**
    * Creates the calls to the specialized function by calling the different versions of the method depending on the classTag
    * if (classTag == manifest[Int]) callIntMethod(...)
    * else if (classTag == manifest[Double]) callDoubleMethod(...)
    * else if (classTag == manifest[Boolean]) callBooleanMethod(...)
    * else callGenericMethod(...)
    * @param c: context of the macro
    * @param classTag: expression that represents the classTag
    * @param typeOf_T: The type being specialized
    * @param typeOf_f: The return type of the block being specialized.
    * @param mapping: represents the arguments, where the keys are the names of the arguments and the mapping contains a 3-tuple with the actual type in position ._2 and the actual tree representing the reference to that argument in position ._3.
    * @param specMethodNamesAndTypes: A mapping from (Int,Boolean,...) to a 2-tuple that has the name of the method in the position ._1.
    * @param specMethodNameGeneric: Name of the fall-back method that gets executed for any non specialized type parameter.
    * @return expression containing the all the different callers
    */
   @inline private def createSpecCallers[T](c: Context)(classTag: c.Expr[ClassTag[T]], typeOf_T: c.Type, typeOf_f: c.Type, mapping: SortedMap[String, (String, c.Type, c.Tree)], specMethodNamesAndTypesList: List[(Specializable, c.TermName, c.Type)], specMethodNameGeneric: c.Name) = {
      import c.universe._

      def createSpecCaller(name: Name, tpe: Type) = {
         c.Expr(
            Apply(
               Ident(name),
               (for (key <- mapping.keys) yield {
                  castTree(c)(
                     c.resetAllAttrs(mapping(key)._3),
                     TypeTree().setType(mapping(key)._2).substituteTypes(List(typeOf_T.typeSymbol), List(tpe)).tpe)
               }).toList))
      }

      val callMethods = SortedMap((specMethodNamesAndTypesList map { case (tp, name, tpe) => tp -> createSpecCaller(name, tpe) }): _*)
      val callGenericMethod = c.Expr(Apply(Ident(specMethodNameGeneric), Nil))

      val manifestsExpr = SortedMap[Specializable, c.Expr[Any]](
         Int -> reify { ManifestFactory.Int },
         Long -> reify { ManifestFactory.Long },
         Double -> reify { ManifestFactory.Double },
         Float -> reify { ManifestFactory.Float },
         Short -> reify { ManifestFactory.Short },
         Char -> reify { ManifestFactory.Char },
         Byte -> reify { ManifestFactory.Byte },
         Boolean -> reify { ManifestFactory.Boolean },
         Unit -> reify { ManifestFactory.Unit })

      val callersBlock = callMethods.keys.foldRight[c.Expr[Any]](callGenericMethod)((tpe, expr) => reify { if (classTag.splice == manifestsExpr(tpe).splice) callMethods(tpe).splice else expr.splice })

      castTree(c)(callersBlock.tree, typeOf_f)
   }

   /**
    * Cast a c.Expr to a given type using isInstanceOf[].
    * @param c: The context of the macro.
    * @param expr: The expression to be casted.
    * @param tpe: Type of the casting.
    * @return the expression casted to tpe.
    */
   @inline private def castExpr(c: Context)(expr: c.Expr[Any], tpe: c.Type): c.Expr[Any] = c.Expr[Any](castTree(c)(expr.tree, tpe))

   /**
    * Cast a c.Tree to a given type using isInstanceOf[].
    * @param c: The context of the macro.
    * @param tree: The tree to be casted.
    * @param tpe: Type of the casting.
    * @return the tree casted to tpe.
    */
   @inline private def castTree(c: Context)(tree: c.Tree, tpe: c.Type): c.Tree = {
      import c.universe._
      TypeApply(Select(tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe)))
   }

   /**
    * @param c
    * @param typeTree
    * @param typeOf_T
    * @param typeOf_Spec
    * @return
    */
   @inline private def tranformTypeTree(c: Context)(typeTree: c.Tree, typeOf_T: c.Type, typeOf_Spec: c.Type): c.Tree = {
      import c.universe._
      TypeTree().setType(typeTree.tpe.widen.substituteTypes(List(typeOf_T.typeSymbol), List(typeOf_Spec)))
   }

}