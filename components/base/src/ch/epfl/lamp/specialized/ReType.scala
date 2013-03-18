package ch.epfl.lamp.specialized

import scala.reflect.macros.Context

object ReType {

   def reType(c: Context)(from: c.Type, to: c.Type, in: c.Type): c.Tree = {
      c.universe.TypeTree().setType(in.substituteTypes(List(from.typeSymbol), List(to)))
   }

}