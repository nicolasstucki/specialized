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
object SpecializedDirectTest extends DirectTest {

   override lazy val testPath = io.File("tmp/")
   override lazy val testOutput = io.Directory("tmp/")

   override def extraSettings = "-uniqid"
   
   def code = """object A { println("fjadsl") }"""
      
   def show: Unit = {
      compile()
   }
}

