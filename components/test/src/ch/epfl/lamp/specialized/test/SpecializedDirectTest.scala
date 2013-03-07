package ch.epfl.lamp.specialized.test

import scala.tools.partest._

import scala.tools.nsc._
import nest.FileUtil._
import io.Directory

/**
 * A trait for testing icode.  All you need is this in a
 *  partest source file:
 *  {{{
 *    object Test extends IcodeTest
 *  }}}
 *  And then the generated output will be the icode for everything
 *  in that file.  See source for possible customizations.
 */
object SpecializedDirectTest extends IcodeTest {
   override val printIcodeAfterPhase = "specialize"
   override val testPath = io.File("test/in")
   override val testOutput = io.Directory("test/out")
}

