package ch.epfl.lamp.specialized

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.Specializable._
import scala.reflect.ManifestFactory
import scala.collection.{ SortedMap => Map }

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
    val types = c.universe.reify { Specializable.Primitives }
    impl_specialized_group[T](c)(types)(expr_f)(classTag)(typetagT)
  }

  /**
   * Macro implementation of specialized[T](specializedGroup){...}
   * @param c: Context
   * @param specGroupExpr: Types to be specialized. If empty, default specialization is used (Int,Double,Boolean)
   * @param expr_f: Code inside the specialized block
   * @param classTag: ClassTag of the type being specialized
   * @param typetagT: Implicit WeakTypeTag of the type being specialized
   * @return: Code inside the specialized block with specialization
   */
  def impl_specialized_group[T](c: Context)(specGroup: c.Expr[SpecializedGroup])(expr_f: c.Expr[Any])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
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
    impl_specialized[T](c)(specList: _*)(expr_f)(classTag)(typetagT)
  }

  /**
   * Macro implementation of specialized[T](...){...}
   * @param c: Context
   * @param types: Types to be specialized. If empty, default specialization is used (Int,Double,Boolean)
   * @param expr_f: Code inside the specialized block
   * @param classTag: ClassTag of the type being specialized
   * @param typetagT: Implicit WeakTypeTag of the type being specialized
   * @return: Code inside the specialized block with specialization
   */
  def impl_specialized[T](c: Context)(types: c.Expr[Specializable]*)(expr_f: c.Expr[Any])(classTag: c.Expr[ClassTag[T]])(implicit typetagT: c.WeakTypeTag[T]): c.Expr[Any] = {
    import c.universe._

    // TYPE PARAMETER CHECKS
    val typeOf_T = typetagT.tpe
    val typeOf_f = expr_f.actualType

    val typesList = types.toList match {
      case list @ _ :: _ => list map (tpExpr => c eval c.Expr[Specializable](c resetAllAttrs tpExpr.tree))
      case Nil => List(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)
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

    // RETRIEVE DEFINITIONS AND USES OF TERMS THAT HAVE T IN THE TYPE
    val mapping = getTemsMapping(c)(expr_f, typeOf_T)

    val typesToTypeTree = getSpecializableTypesMap(c) filterKeys (typesList.contains(_))

    // CREATE NAME FOR THE NEW METHOD
    val DefDef(_, encMethName, _, _, _, _) = c.enclosingMethod
    val specMethodName = c.fresh(newTermName(f"${encMethName}_specialized"))

    // CREATE DISPATCH TREE
    def createSpecCaller(typeOfCall: Option[Type]) = {
      val methodCall = typeOfCall match {
        case Some(tpe) => TypeApply(Ident(newTermName(specMethodName.toString)), List(Ident(tpe.typeSymbol)))
        case None => Ident(newTermName(specMethodName.toString))
      }
      def createParameterTree(key: String) = typeOfCall match {
        case Some(tpe) =>
          castTree(c)(
            mapping(key)._3,
            TypeTree().setType(mapping(key)._2).substituteTypes(List(typeOf_T.typeSymbol), List(tpe)).tpe)
        case None => mapping(key)._3
      }
      c.Expr(Apply(methodCall, mapping.keys.toList.map(createParameterTree(_))))
    }

    val callMethods = Map((typesToTypeTree.keys.toList map { tp => tp -> createSpecCaller(Some(typesToTypeTree(tp))) }): _*)
    val callGenericMethod = createSpecCaller(None)

    val manifestsExpr = getManifestExprMap(c)

    val callersBlock = callMethods.keys.foldRight[c.Expr[Any]](callGenericMethod)((tpe, expr) => reify { if (classTag.splice == manifestsExpr(tpe).splice) callMethods(tpe).splice else expr.splice }).tree

    // CREATE SPECIALIZED METHOD TREE
    object fieldToIdents extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case select: Select =>
          mapping.get(select.toString) match {
            case Some((newName, _, _)) => Ident(newTermName(newName))
            case None => super.transform(tree)
          }
        case _ => super.transform(tree)
      }
    }

    val newBody = fieldToIdents.transform(expr_f.tree)

    val vparamss_str = (mapping.values.toList map { case (param_name, param_type, _) => f"$param_name: $param_type" }).mkString(", ")
    val at_spec_params_str = typesToTypeTree.keys.map(_.toString.drop(13)).mkString(", ")
    val cast_back_str = if (typeOf_f.contains(typeOf_T.typeSymbol)) s".asInstanceOf[$typeOf_f]" else ""

    // TODO: Wrapping the spec method inside an object is a workaround issue SI-7344, this should be removed as soon as the issue is resolved.
    val spec_obj = c.fresh("SpecObject").toString

    // JOIN EVERYTHING TOGETHER
    // Here parse is used instead of reify to ensure that all symbols are erased from the tree
    val newTreeStr = s"""{
        	object $spec_obj {
				def $specMethodName[@specialized($at_spec_params_str) $typeOf_T]($vparamss_str): $typeOf_f = { $newBody }
			}
			import $spec_obj._
			import scala.reflect.ManifestFactory
			$callersBlock
    	}$cast_back_str""".replaceAllLiterally("scala.this.Predef.", "")

    val newExpr = c.Expr[Any](c.typeCheck(c.parse(newTreeStr), typeOf_f))
    // c.warning(classTag.tree.pos, "new expr: " + show(newExpr))
    // c.warning(classTag.tree.pos, "new expr: " + showRaw(newExpr))
    newExpr
  }

  /**
   * Mapping that contains all information needed about the all the arguments that will be needed for the specialized methods.
   * The keys of the mapping contain the original names of the argument. Each is mapped into a 3-tuple that contains
   * the new name (or the same as the key if renaming is unecesary), the widen type of the argument and the reference to the argument itself.
   * @param c: context of the macro
   * @param expr: the body of the specialized expression.
   * @param typeOf_T: the type being specialized.
   * @return the mapping of all arguments needed for specialization
   */
  private[this] def getTemsMapping[T](c: Context)(expr: c.Expr[Any], typeOf_T: c.Type): Map[String, (String, c.Type, c.Tree)] = {
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
          case select @ Select(This(_), _) if typeHasT(select) =>
            select.tpe.widen match {
              case _: TypeRef =>
                val strRep = select.toString
                if (!fieldsMapping.contains(strRep))
                  fieldsMapping += (strRep -> (c.fresh(newTermName(strRep.toString.replace(".", "_"))).toString, select.tpe.widen, select))
              case _ =>
            }
          case select @ Select(Ident(termName), _) if typeHasT(select) && !valDefsInScope(termName.toString) =>
            select.tpe.widen match {
              case _: TypeRef =>
                val strRep = select.toString
                if (!fieldsMapping.contains(strRep))
                  fieldsMapping += (strRep -> (c.fresh(newTermName(strRep)).toString, select.tpe.widen, select))
              case _ =>
            }
          case ValDef(_, name, _, _) =>
            valdefInside += name.toString
            valDefsInScope = valDefsInScope + name.toString
          case ident @ Ident(name) if typeHasT(ident) =>
            val strRep = name.toString
            if (!fieldsMapping.contains(strRep))
              fieldsMapping += (strRep -> (strRep, ident.tpe.widen, ident))
          case DefDef(_, name, _, vparamss, tpt, _) if typeHasT(tpt) =>
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

    // TODO: may need to go also in the enclosing method of the enclosing method if it exists
    enclosingMethodTraverser.traverse(c.enclosingMethod)
    val ClassDef(_, _, _, Template(parents, self, body)) = c.enclosingClass
    val vardefsInClass = body collect { case ValDef(mods, name, _, _) if mods.hasFlag(Flag.MUTABLE) => name.toString }

    fieldsMapping --= vardefs.keys
    fieldsMapping --= vardefsInClass

    Map[String, (String, c.Type, c.Tree)](fieldsMapping.toList: _*)
  }

  /**
   * @param c
   * @return
   */
  private[this] def getSpecializableTypesMap(c: Context): Map[Specializable, c.Type] = {
    import c.universe._
    Map[Specializable, Type](
      Int -> typeOf[Int],
      Long -> typeOf[Long],
      Double -> typeOf[Double],
      Float -> typeOf[Float],
      Char -> typeOf[Char],
      Short -> typeOf[Short],
      Byte -> typeOf[Byte],
      Boolean -> typeOf[Boolean],
      Unit -> typeOf[Unit])
  }

  /**
   * @param c
   * @return
   */
  private[this] def getManifestExprMap(c: Context): Map[Specializable, c.Expr[Any]] = {
    import c.universe._
    Map[Specializable, c.Expr[Any]](
      Int -> reify { ManifestFactory.Int },
      Long -> reify { ManifestFactory.Long },
      Double -> reify { ManifestFactory.Double },
      Float -> reify { ManifestFactory.Float },
      Short -> reify { ManifestFactory.Short },
      Char -> reify { ManifestFactory.Char },
      Byte -> reify { ManifestFactory.Byte },
      Boolean -> reify { ManifestFactory.Boolean },
      Unit -> reify { ManifestFactory.Unit })
  }

  /**
   * Cast a c.Tree to a given type using isInstanceOf[] by wrapping the tree into it.
   * @param c: The context of the macro.
   * @param tree: The tree to be casted.
   * @param tpe: Type of the casting.
   * @return the original tree wrapped in a cast to tpe.
   */
  @inline private[this] def castTree(c: Context)(tree: c.Tree, tpe: c.Type): c.Tree = {
    import c.universe._
    TypeApply(Select(tree, newTermName("asInstanceOf")), List(TypeTree().setType(tpe)))
  }

}