package ch.epfl.lamp.specialized.benchmark.cases

trait BenchmarkApi {
   def test: Any
   def testUnrolled: Any
   def testSpecialized: Any
   def testSpecializedBlock: Any
}