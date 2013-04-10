
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag] {

   def mPrimitives() = {
      specialized[T](Specializable.Primitives) {
         println
      }
   }
   
   def mEverything() = {
      specialized[T](Specializable.Everything) {
         println
      }
   }

   def mBits32AndUp() = {
      specialized[T](Specializable.Bits32AndUp) {
         println
      }
   }
   
   def mIntegral() = {
      specialized[T](Specializable.Integral) {
         println
      }
   }
   
   def mAllNumeric() = {
      specialized[T](Specializable.AllNumeric) {
         println
      }
   }
   
   def mBestOfBreed() = {
      specialized[T](Specializable.BestOfBreed) {
         println
      }
   }
}