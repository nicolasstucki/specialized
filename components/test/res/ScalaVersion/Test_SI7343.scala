import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class Target[@specialized(Int) T] { def foo = 1 }

object Test {

  def spec_mapper[@specialized(Int) T](t: T) = {
    class X extends Target[T]() 
    (new X).foo
  }
}