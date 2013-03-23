package ch.epfl.lamp.specialized.benchmark.tests

trait TestApi {
   def test: Any
   def testUnrolled: Any
   def testSpecialized: Any
   def testSpecializedBlock: Any
}