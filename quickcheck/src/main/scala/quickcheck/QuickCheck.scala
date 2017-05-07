package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck.{Prop, _}
import org.scalacheck.Prop.BooleanOperators
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty),
    for {
      m <- arbitrary[A]
      h <- genHeap
    } yield insert(m, h))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def isSorted(h: H) = {
    @annotation.tailrec
    def go(lastMin: A, h: H): Boolean =
      if (isEmpty(h)) true
      else (ord.compare(lastMin, findMin(h)) <= 0) && go(findMin(h), deleteMin(h))

    isEmpty(h) || go(findMin(h), h)
  }

  property("gen1") = forAll { (h: H, m: A) =>
    val min = if (isEmpty(h)) m else ord.min(m, findMin(h))
    findMin(insert(m, h)) == min
  }

  property("is not empty after insert") = forAll { (h: H, m: A) =>
    !isEmpty(insert(m, h))
  }

  property("min goes first") = forAll { (m: A, n: A) =>
    val h = insert(n, insert(m, empty))
    findMin(h) == (if (ord.compare(m, n) <= 0) m else n)
  }

  property("elements are retrieved in sorted order") = forAll { (h: H) =>
    isSorted(h)
  }

  property("elements are retrieved in sorted order 2") = forAll { (h: H) =>
    val l = toList(h)
    l =? l.sorted
  }

  property("elements are retrieved in sorted order 2") = forAll { (l: List[A]) =>
    val h = toHeap(l)
    toList(h) =? l.sorted
  }

  property("melding of two empty heaps is empty") =
    isEmpty(meld(empty, empty))

  property("melding preserves order") = forAll { (h1: H, h2: H) =>
    isSorted(meld(h1, h2))
  }

  property("minimum of melded heap is minimum of one of the original heaps") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) Prop.throws(classOf[NoSuchElementException]) { findMin(melded) }
    else if (isEmpty(h1)) findMin(melded) == findMin(h2)
    else if (isEmpty(h2)) findMin(melded) == findMin(h1)
    else findMin(melded) == ord.min(findMin(h1), findMin(h2))
  }

  property("findMin throws exception for empty heap") =
    Prop.throws(classOf[NoSuchElementException]) { findMin(empty) }

  property("deleteMin throws exception for empty heap") =
    Prop.throws(classOf[NoSuchElementException]) { deleteMin(empty) }

  property("deleteMin preserves order") = forAll { (h: H) =>
    isEmpty(h) || isSorted(deleteMin(h))
  }

  property("deleteMin deletes") = forAll { (h: H, m: A, n: A) =>
    isEmpty(deleteMin(deleteMin(insert(m, insert(n, h))))) == isEmpty(h)
  }

  property("deleteMin only deletes one min") = forAll { (h: H) =>
    Prop(!isEmpty(h)) ==> (findMin(deleteMin(insert(findMin(h), h))) == findMin(h))
  }

  property("deleteMin does not break heap") = forAll { (h: H) =>
    Prop(!isEmpty(h)) ==> (findMin(insert(findMin(h),deleteMin(h))) == findMin(h))
  }

  def toHeap(l: List[A]) = {
    @annotation.tailrec
    def go(t: List[A], h: H): H = t match {
      case Nil => h
      case a :: as => go(as, insert(a, h))
    }
    go(l, empty)
  }

  def toList(h: H): List[A] = {
    @annotation.tailrec
    def go(t: H, acc: List[A]): List[A] = {
      if (isEmpty(t)) acc
      else go(deleteMin(t), findMin(t) :: acc)
    }
    go(h, Nil).reverse
  }

}
