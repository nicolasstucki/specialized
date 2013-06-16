
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag](val dummy: T) {

   def mBoolean() = {
      specialized[T](Boolean) {
         dummy
      }
   }

   def mByte() = {
      specialized[T](Byte) {
         dummy
      }
   }

   def mChar() = {
      specialized[T](Char) {
         dummy
      }
   }

   def mDouble() = {
      specialized[T](Double) {
         dummy
      }
   }

   def mFloat() = {
      specialized[T](Float) {
         dummy
      }
   }

   def mInt() = {
      specialized[T](Int) {
         dummy
      }
   }

   def mLong() = {
      specialized[T](Long) {
         dummy
      }
   }

   def mShort() = {
      specialized[T](Short) {
         dummy
      }
   }
   
   def mUnit() = {
      specialized[T](Unit) {
         dummy
      }
   }
}