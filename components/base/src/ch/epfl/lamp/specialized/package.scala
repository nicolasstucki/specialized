package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.Specializable._
import scala.reflect.ManifestFactory
import scala.collection.{ SortedMap => Map }

/** @author Nicolas Stucki
  *
  */
final object `package` {

  def specialized[T]: Specialized[T] = new SpecializedImpl[T]

  private[this] final class SpecializedImpl[T] extends Specialized[T]

  trait Specialized[T] {
    /** Specialized block
      * @param code: Code inside the specialized block
      * @param classTag: Implicit ClassTag of the type being specialized
      * @return: Specialized version of the block
      */
    def apply[R](code: => R)(implicit classTag: ClassTag[T]): R = macro Macros.impl_specialized_default_types[T, R]

    /** Specialized block
      * @param specGroup: types that will be specialized
      * @param code: Code inside the specialized block
      * @param classTag: Implicit ClassTag of the type being specialized
      * @return: Specialized version of the block
      */
    def apply[R](specGroup: SpecializedGroup)(code: => R)(implicit classTag: ClassTag[T]): R = macro Macros.impl_specialized_group[T, R]

    /** Specialized block
      * @param types: types that will be specialized
      * @param code: Code inside the specialized block
      * @param classTag: Implicit ClassTag of the type being specialized
      * @return: Specialized version of the block
      */
    def apply[R](types: Specializable*)(code: => R)(implicit classTag: ClassTag[T]): R = macro Macros.impl_specialized[T, R]
  }

  private[this] final object Macros {
    /** Implicit order for Specializable objects
      */
    private[this] implicit object SpecializableOrdering extends Ordering[Specializable] {
      def compare(a: Specializable, b: Specializable) = a.toString compare b.toString
    }

    /** Macro implementation of specialized[T]{...}
      * @param c: Context
      * @param code: Code inside the specialized block
      * @param classTag: ClassTag of the type being specialized
      * @param typetagT: Implicit WeakTypeTag of the type being specialized
      * @return: Code inside the specialized block with specialization
      */
    def impl_specialized_default_types[T, R](c: Context)(code: c.Expr[R])(classTag: c.Expr[scala.reflect.ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T], typetagR: c.WeakTypeTag[R]): c.Expr[R] = {
      val types = c.universe.reify { Specializable.Primitives }
      impl_specialized_group[T, R](c)(types)(code)(classTag)
    }

    /** Macro implementation of specialized[T](specializedGroup){...}
      * @param c: Context
      * @param specGroupExpr: Types to be specialized. If empty, default specialization is used (Int,Double,Boolean)
      * @param code: Code inside the specialized block
      * @param classTag: ClassTag of the type being specialized
      * @param typetagT: Implicit WeakTypeTag of the type being specialized
      * @return: Code inside the specialized block with specialization
      */
    def impl_specialized_group[T, R](c: Context)(specGroup: c.Expr[SpecializedGroup])(code: c.Expr[R])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T], typetagR: c.WeakTypeTag[R]): c.Expr[R] = {
      import c.universe._
      val group = c eval c.Expr[SpecializedGroup](c resetAllAttrs specGroup.tree)
      val specList = (group match {
        case Primitives => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char }, reify { Float }, reify { Double }, reify { Boolean }, reify { Unit })
        case Everything => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char }, reify { Float }, reify { Double }, reify { Boolean }, reify { Unit } /*, reify { AnyRef }*/ )
        case Bits32AndUp => List(reify { Int }, reify { Long }, reify { Float }, reify { Double })
        case Integral => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char })
        case AllNumeric => List(reify { Byte }, reify { Short }, reify { Int }, reify { Long }, reify { Char }, reify { Float }, reify { Double })
        case BestOfBreed => List(reify { Int }, reify { Double }, reify { Boolean }, reify { Unit } /*, reify { AnyRef }*/ )
        case _ => c.warning(classTag.tree.pos, s"Specialized is not able to handle '${show(group)}'. Use the list of types instead."); Nil
      })
      impl_specialized[T, R](c)(specList: _*)(code)(classTag)
    }

    /** Macro implementation of specialized[T](...){...}
      * @param c: Context
      * @param types: Types to be specialized. If empty, default specialization is used (Int,Double,Boolean)
      * @param code: Code inside the specialized block
      * @param classTag: ClassTag of the type being specialized
      * @param typetagT: Implicit WeakTypeTag of the type being specialized
      * @return: Code inside the specialized block with specialization
      */
    def impl_specialized[T, R](c: Context)(types: c.Expr[Specializable]*)(code: c.Expr[R])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T], typetagR: c.WeakTypeTag[R]): c.Expr[R] = {
      import c.universe._

      // TYPE PARAMETER CHECKS
      val typeOfT = typetagT.tpe
      val typeOfR = typetagR.tpe

      val pos = classTag.tree.pos

      val typesList = types.toList match {
        case Nil => List(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)
        case list => list map (tpExpr => c eval c.Expr[Specializable](c resetAllAttrs tpExpr.tree))
      }

      // Check if T is a valid type parameter
      typeOfT match {
        case TypeRef(NoPrefix, _, _) => // Do nothing, typeOf_T is OK
        case TypeRef(_, _, _) => {
          // This happens when:
          // 1. specialized {...} is used and there is no type parameter with a ClassTag in scope 
          // 2. or there is more than one type parameter have manifests and therefore the type could not be inferred.
          // 3. The type parameter is not a type parameter of the enclosing context, examples: specialized[Int] {...}, specialized[Any] {...}, specialized[Array[Int]] {...}, specialized[T] {...}, ...   
          c.error(pos, "Specify type parameter using: specialized[T] {...}, T must be a type parameter of the enclosing context and it must have a ClassTag." +
            " Type patameter must be on top level, example: if you want to specialize an Array[T] use specialize[T] {...}.")
          return code
        }
      }

      // Do not let the use of Return statements
      if (treeHasReturns(c)(code.tree)) {
        c.error(pos, "specialized[T] {...} doesn't suport 'return'")
        return code
      }

      // RETRIEVE DEFINITIONS AND USES OF TERMS THAT HAVE T IN THE TYPE
      val (variableMapping, outsideVarUsedInside) = getVariablesMapping(c)(code, typeOfT)
      if (outsideVarUsedInside) {
        c.error(pos, "specialized[T] {...} can't access a var defined outside it.")
        return code
      }
      // c.echo(pos, "mapping: " + variableMapping)

      val typesToTypeTree = getSpecializableTypesMap(c) filterKeys (typesList.contains(_))

      // CREATE NAME FOR THE NEW METHOD
      val DefDef(_, encMethName, _, _, _, _) = c.enclosingMethod
      val specMethodName = c.fresh(newTermName(s"${encMethName}_specialized"))

      // CREATE DISPATCH TREE
      def createSpecCaller(typeOfCall: Option[Type]) = {
        val methodCall = typeOfCall match {
          case Some(tpe) => TypeApply(Ident(newTermName(specMethodName.toString)), List(Ident(tpe.typeSymbol)))
          case None => Ident(newTermName(specMethodName.toString))
        }
        def createParameterTree(key: String) = {
          typeOfCall match {
            case Some(tpe) =>
              val newType = TypeTree().setType(variableMapping(key)._2).substituteTypes(List(typeOfT.typeSymbol), List(tpe)).tpe
              castedTree(c)(variableMapping(key)._3, newType)
            case None => variableMapping(key)._3
          }
        }
        c.Expr[Any](Apply(methodCall, variableMapping.keys.toList.map(createParameterTree(_))))
      }

      val callMethods = Map((typesToTypeTree.keys.toList map { tp => tp -> createSpecCaller(Some(typesToTypeTree(tp))) }): _*)
      val callGenericMethod = createSpecCaller(None)

      val manifestsExpr = getManifestExprMap(c)

      val callersBlockTree = callMethods.keys.foldRight[c.Expr[Any]](callGenericMethod)((tpe, expr) => reify { if (classTag.splice == manifestsExpr(tpe).splice) callMethods(tpe).splice else expr.splice }).tree

      // CREATE SPECIALIZED METHOD TREE
      object fieldToIdents extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case select: Select =>
            variableMapping.get(select.toString) match {
              case Some((newName, _, _)) => Ident(newTermName(newName))
              case None => super.transform(tree)
            }
          case _ => super.transform(tree)
        }
      }

      val newBodyTree = fieldToIdents.transform(code.tree)

      //        c.echo(pos, "tree: " + show(code.tree))
      //    c.echo(pos, "tree: " + showRaw(code.tree))

      // These parts are Strings because they will be used inside a c.parse
      val vparamss_str = (variableMapping.values.toList map { case (param_name, param_type, _) => s"$param_name: $param_type" }).mkString(", ")
      val at_spec_params_str = typesToTypeTree.keys.map(_.toString.drop(13)).mkString(", ")
      val cast_back_str = if (typeOfR.contains(typeOfT.typeSymbol)) s".asInstanceOf[$typeOfR]" else ""

      // TODO: Wrapping the spec method inside an object is a workaround issue SI-7344, this should be removed as soon as the issue is resolved.
      val spec_obj = c.fresh("SpecObject")

      // JOIN EVERYTHING TOGETHER

      //// Here parse is used instead of reify to ensure that all symbols are erased from the tree
      val newTreeTemplate_str = s"""{
                	object $spec_obj {
        				def $specMethodName[@specialized($at_spec_params_str) $typeOfT]($vparamss_str): $typeOfR = { ??? }
        			}
        			import $spec_obj._
        			import scala.reflect.ManifestFactory
        			${callersBlockTree}
            	}$cast_back_str"""

      object bodyInliner extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Ident(name) if name.toString == "$qmark$qmark$qmark" => c.resetAllAttrs(newBodyTree)
          case _ => super.transform(tree)
        }
      }

      //    c.echo(pos, "template: " + newTreeTemplate_str)

      // Reset all symbols from the tree
      clearAllSymbolsFromTree(c)(newBodyTree)

      // Inline the body in the place where ??? is in the  template
      val newTree = bodyInliner.transform(c.parse(newTreeTemplate_str))

      //    c.echo(pos, "newTree: " + show(newTree))
      //    c.echo(pos, "newTree: " + showRaw(newTree))

      // Typecheck new tree and wrap it in an expresion
      val newExpr = c.Expr[R](c.typeCheck(newTree, typeOfR))

      //    c.echo(pos, "newExpr: " + show(newExpr))
      //    c.echo(pos, "newExpr: " + showRaw(newExpr))

      newExpr
    }

    private[this] def getVariablesMapping[T](c: Context)(code: c.Expr[Any], typeOf_T: c.Type): (Map[String, (String, c.Type, c.Tree)], Boolean) = {
      import c.universe._

      val fieldsMapping = scala.collection.mutable.Map.empty[String, (String, Type, Tree)]
      var outsideVarUsedInside = false

      def typeHasT(tree: Tree): Boolean = tree.tpe != null && tree.tpe.widen.exists(_ == typeOf_T)

      object bodyTraverser extends Traverser {
        // Keeps track of all identifier definitions that where done 
        // inside the code and are visible at some sub-tree
        var defsInScope = Set.empty[String]

        override def traverse(tree: Tree) {

          val oldValDefsInScope = defsInScope

          tree match {
            case Block(stats, expr) =>
              stats foreach {
                case ValDef(_, name, _, rhs) =>
                  defsInScope = defsInScope + name.toString
                  traverse(rhs)
                case DefDef(_, name, _, vparamss, _, rhs) =>
                  val old = defsInScope
                  defsInScope = defsInScope ++ (vparamss flatMap (_ map { case ValDef(_, name, _, _) => name.toString }))
                  traverse(rhs)
                  defsInScope = old
                case tree =>
                  traverse(tree)
              }
              traverse(expr)
            case select @ Select(This(_), _) if typeHasT(select) && select.symbol.isTerm =>
              select.tpe.widen match {
                case _: TypeRef =>
                  val strRep = select.toString
                  if (!fieldsMapping.contains(strRep))
                    fieldsMapping += (strRep -> (c.fresh(newTermName(strRep.toString.replace(".", "_"))).toString, select.tpe.widen, select))
                case _ =>
              }
            case select @ Select(Ident(termName), _) if typeHasT(select) && select.symbol.isTerm && !defsInScope(termName.toString) =>
              select.tpe.widen match {
                case _: TypeRef =>
                  val strRep = select.toString
                  val strRepName = termName.toString
                  if (!fieldsMapping.contains(strRep) && !defsInScope.contains(strRepName)) {
                    fieldsMapping += (strRep -> (c.fresh(newTermName(strRep)).toString, select.tpe.widen, select))
                  }
                case _ =>
              }
            case ValDef(_, name, _, _) =>
              defsInScope = defsInScope + name.toString
            case ident @ Ident(name) if typeHasT(ident) && ident.symbol.isTerm =>
              val strRep = name.toString
              val term = ident.symbol.asTerm
              if (!defsInScope.contains(strRep)) {
                if (term.isVal && strRep != "_") {
                  fieldsMapping += (strRep -> (strRep, ident.tpe.widen, ident))
                } else if (term.isVar) {
                  outsideVarUsedInside = true
                }
              }
            case DefDef(_, name, _, vparamss, tpt, _) if typeHasT(tpt) =>
              defsInScope = defsInScope ++ (vparamss flatMap (_ map { case ValDef(_, name, _, _) => name.toString }))
            case _ =>
          }

          super.traverse(tree)

          defsInScope = oldValDefsInScope
        }
      }

      bodyTraverser.traverse(code.tree)

      (Map[String, (String, c.Type, c.Tree)](fieldsMapping.toList: _*), outsideVarUsedInside)
    }

    /** @param c
      * @return
      */
    private[this] def getSpecializableTypesMap(c: Context): Map[Specializable, c.Type] = {
      import c.universe._
      Map[Specializable, Type](Int -> typeOf[Int], Long -> typeOf[Long], Double -> typeOf[Double], Float -> typeOf[Float],
        Char -> typeOf[Char], Short -> typeOf[Short], Byte -> typeOf[Byte], Boolean -> typeOf[Boolean], Unit -> typeOf[Unit])
    }

    /** @param c
      * @return
      */
    private[this] def getManifestExprMap(c: Context): Map[Specializable, c.Expr[Any]] = {
      import c.universe._
      Map[Specializable, c.Expr[Any]](
        Int -> reify { ManifestFactory.Int }, Long -> reify { ManifestFactory.Long }, Double -> reify { ManifestFactory.Double },
        Float -> reify { ManifestFactory.Float }, Short -> reify { ManifestFactory.Short }, Char -> reify { ManifestFactory.Char },
        Byte -> reify { ManifestFactory.Byte }, Boolean -> reify { ManifestFactory.Boolean }, Unit -> reify { ManifestFactory.Unit })
    }

    /** Cast a c.Tree to a given type using isInstanceOf[] by wrapping the tree into it.
      * @param c: The context of the macro.
      * @param tree: The tree to be casted.
      * @param tpe: Type of the casting.
      * @return the original tree wrapped in a cast to tpe.
      */
    private[this] def castedTree(c: Context)(tree: c.Tree, tpe: c.Type): c.Tree = {
      import c.universe._
      TypeApply(Select(tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe)))
    }

    private[this] def clearAllSymbolsFromTree(c: Context)(tree: c.Tree) {
      import c.universe._
      tree.foreach(subtree => {
        // Reset types
        subtree match {
          case EmptyTree => // Do nothing
          case _ => subtree.tpe = null
        }
        // Reset symbols
        subtree match {
          case _: RefTree => subtree.symbol = null
          case _ => // Do nothing
        }
      })
    }

    private[this] def treeHasReturns(c: Context)(tree: c.Tree): Boolean = {
      import c.universe._
      object findReturns extends Traverser {
        var found = false
        override def traverse(tree: Tree) = tree match {
          case Return(_) => found = true
          case _ if !found => super.traverse(tree)
        }
      }
      findReturns.traverse(tree)
      findReturns.found
    }
  }
}