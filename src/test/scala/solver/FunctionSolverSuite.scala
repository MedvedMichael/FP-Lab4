package solver

import org.junit.{Assert, Rule, Test}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Test.{Failed, PropException, Result, check}
import quickcheck.QuickCheckHeap

class FunctionSolverSuite {
  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq: _*)
  @Test def `Function Solver works correctly`(): Unit =
    Assert.assertTrue(
      check(asProp(FunctionSolver))(identity).passed
    )

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
