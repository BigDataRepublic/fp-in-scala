package nl.bdr.fpis.p1

import org.scalatest._

import Chapter2._


class Chapter2Spec extends WordSpec with Matchers {
  "The fib function" when {
    "given 0" should {
      "return 0" in {
        fib(0) should be(0)
      }
    }

    "given 1" should {
      "return 1" in {
        fib(1) should be(1)
      }
    }

    "given 2" should {
      "return 1" in {
        fib(2) should be(1)
      }
    }

    "given 3" should {
      "return 2" in {
        fib(3) should be(2)
      }
    }

    "given 4" should {
      "return 3" in {
        fib(4) should be(3)
      }
    }
  }

  "the isSorted function" when {
    "given an empty list" should {
      "return true" in {
        isSorted(Array.empty[Int], (x: Int, y: Int) => x <= y) should be(true)
      }
    }

    "given a list with one element" should {
      "return true" in {
        isSorted(Array(1), (x: Int, y: Int) => x <= y) should be(true)
      }
    }

    "given a list with multiple elements" should {
      "return true if the elements are ordered" in {
        isSorted(Array(1, 2), (x: Int, y: Int) => x <= y) should be(true)
      }

      "return false if the elements are not ordered" in {
        isSorted(Array(2, 1), (x: Int, y: Int) => x <= y) should be(false)
      }
    }
  }
}
