package ch.epfl.lamp.specialized.test

object `package` {

   def specialized[T: Manifest](f: =>Unit): Unit = f
   
   
}