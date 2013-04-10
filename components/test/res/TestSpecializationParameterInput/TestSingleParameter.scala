
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag] {

   def mBoolean() = {
      specialized[T](Boolean) {
         println
      }
   }

   def mByte() = {
      specialized[T](Byte) {
         println
      }
   }

   def mChar() = {
      specialized[T](Char) {
         println
      }
   }

   def mDouble() = {
      specialized[T](Double) {
         println
      }
   }

   def mFloat() = {
      specialized[T](Float) {
         println
      }
   }

   def mInt() = {
      specialized[T](Int) {
         println
      }
   }

   def mLong() = {
      specialized[T](Long) {
         println
      }
   }

   def mShort() = {
      specialized[T](Short) {
         println
      }
   }
   
   def mUnit() = {
      specialized[T](Unit) {
         println
      }
   }
}