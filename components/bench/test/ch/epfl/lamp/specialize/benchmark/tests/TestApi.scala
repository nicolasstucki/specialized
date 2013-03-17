package ch.epfl.lamp.specialize.benchmark.tests

trait TestApi {
   def test: Any
   def testUnrolled: Any
   def testSpecialized: Any
}