
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag](val dummy: T) {

   def mPrimitives() = {
      specialized[T](Specializable.Primitives) {
         dummy
      }
   }
   
   def mEverything() = {
      specialized[T](Specializable.Everything) {
         dummy
      }
   }

   def mBits32AndUp() = {
      specialized[T](Specializable.Bits32AndUp) {
         dummy
      }
   }
   
   def mIntegral() = {
      specialized[T](Specializable.Integral) {
         dummy
      }
   }
   
   def mAllNumeric() = {
      specialized[T](Specializable.AllNumeric) {
         dummy
      }
   }
   
   def mBestOfBreed() = {
      specialized[T](Specializable.BestOfBreed) {
         dummy
      }
   }
}