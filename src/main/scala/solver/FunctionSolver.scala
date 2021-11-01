package solver

import scala.annotation.tailrec
import org.scalacheck._
import Arbitrary._
import Prop._

import scala.Function.const


object FunctionSolver extends Properties("Solver") {
  def calculateFunction5: PartialFunction[(Double, Double), Double] = {
    case (x, k) if x != 10 =>
      if (x > 10)
        sum(x, 1, 8)
      else
        k * scala.math.pow(x, k)
  }

  def sum(x: Double, start: Int, end: Int): Double = {
    @tailrec
    def sumRecurs(x: Double, start: Int, end: Int, result: Double): Double =
      if (start == end)
        result + x * start
      else
        sumRecurs(x, start + 1, end, result + x * start)

    sumRecurs(x, start, end, 0)
  }


  def toList(range: Seq[Int], k: Double): List[Double] =
    range.map(x => (x.toDouble, k)).collect(calculateFunction5).toList


  def liftedCalculateFunction: ((Double, Double)) => Option[Double] =
    calculateFunction5.lift

  def toListLifted(range: Seq[Int], k: Double): List[Option[Double]] =
    range.foldLeft(List[Option[Double]]()) {
      (acc, x) => acc :+ liftedCalculateFunction(x, k)
    }

  val list: List[Option[Double]] = toListLifted(-250 to 250, 2)
  println(list)

  property("list1") = forAll {
    (x: Int) =>
      val k = 2
      (x == 10) ==> {
        liftedCalculateFunction(x, k).isEmpty
      }
      (x > 10) ==> {
        liftedCalculateFunction(x, k).get == sum(x, 1, 8)
      }
      (x < 10) ==> {
        liftedCalculateFunction(x, k).get == k * scala.math.pow(x, k)
      }
  }

}
