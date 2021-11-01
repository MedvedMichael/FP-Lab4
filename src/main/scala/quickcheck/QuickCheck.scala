package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] =
    for {
      list <- Gen.nonEmptyListOf(arbitrary[A])
      h <- list.foldLeft(empty)((acc, curr) => insert(curr, acc))
    } yield h


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  def toList(h: H): List[A] = {
    if (isEmpty(h))
      List()
    else
      findMin(h) +: toList(deleteMin(h))
  }


  property("ord1") = forAll { (h1: H, h2: H) =>
    val list1 = toList(meld(h1, h2))
    val list2 = toList(meld(h2, h1))

    list1.indices.foldLeft(true)((acc, curr) => !acc || list1(curr) == list2(curr))
  }

  property("min2") = forAll { (x: Int, h: H) =>
    if (x < findMin(h))
      findMin(deleteMin(insert(x, h))) == findMin(h)
    else
      findMin(insert(x, h)) == findMin(h)
  }

//  // 1
//  property("min1") = forAll { (a: Int, b: Int) =>
//    val h = insert(b, insert(a, empty))
//    val min = findMin(h)
//    if (a < b)
//      a == min
//    else
//      b == min
//  }
//
//  // 2
//  property("empty1") = forAll { a: Int =>
//    val h = insert(a, empty)
//    isEmpty(deleteMin(h))
//  }
//
//  // 3
//  property("heap1") = forAll { h: H =>
//    @tailrec
//    def checkSort(h: H, last: A): Boolean =
//      isEmpty(h) || (last <= findMin(h) && checkSort(deleteMin(h), findMin(h)))
//
//    checkSort(deleteMin(h), findMin(h))
//  }
//
//  // 4
//  property("union1") = forAll { (h1: H, h2: H) =>
//    val h = meld(h1, h2)
//    val min = findMin(h)
//    findMin(h1) == min || findMin(h2) == min
//  }


  //  property("min1") = forAll { a: Int =>
  //    //    println(a)
  //    val h = insert(a, empty)
  //    //    throw new NoSuchElementException()
  //    findMin(h) == a
  //  }
  //
  //
  //  property("gen1") = forAll { (a: Int) =>
  //    val h = insert(5, empty)
  //    val m = if (isEmpty(h)) 0 else findMin(h)
  //    findMin(insert(m, h)) == m
  //  }

  //
  // WORKING FROM TIME TO TIMES
  //    property("min3") = forAll { h: H =>
  //      val min = findMin(h)
  //      findMin(insert(min + 1, h)) == min
  //    }

}
