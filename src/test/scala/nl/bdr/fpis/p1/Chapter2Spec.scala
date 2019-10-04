package nl.bdr.fpis.p1

import org.scalatest._
import Chapter2._
import nl.bdr.fpis.generators.FPGeneratorConfiguration
import org.scalacheck.Gen.chooseNum
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class Chapter2Spec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks with FPGeneratorConfiguration {
  "The fib function" when {
    "forAll n" should {
      "fib(n) == fib(n - 1) + fib(n + 2)" in {
        forAll(chooseNum(minT = 0, maxT = 4000)) { n: Int =>
          if (n <= 0) fib(n) should be(0)
          else if (n == 1) fib(n) should be(1)
          else fib(n) should be(fib(n - 1) + fib(n - 2))
        }
      }
    }
  }

  "the isSorted function" when {
    "given an empty list" should {
      val emptyList = Array.empty[Int]
      "return true" in {
        isSorted(emptyList, (x: Int, y: Int) => x <= y) should be(true)
      }
    }

    "given a list with one element" should {
      val oneElementList = Array(1)
      "return true" in {
        isSorted(oneElementList, (x: Int, y: Int) => x <= y) should be(true)
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

  "the curry function" when {
    "given a repeat function function" should {

      // Repeats the string a number of times.
      def repeatString(a: String, b: Int): String = Seq.fill(b)(a).mkString("")

      "should curry on the first argument" in {
        val repeatCurried: String => Int => String = curry(repeatString)
        val toodleDoRepeater: Int => String = repeatCurried("toodledo")
        toodleDoRepeater(3) should be("toodledotoodledotoodledo")
      }
    }
  }

  "the uncurry function" when {
    "given a curried minus function" should {
      val curriedMinF: Int => Int => Int = (a: Int) => (b: Int) => a - b
      "should uncurry" in {
        val function: (Int, Int) => Int = uncurry(curriedMinF)
        function(5, 2) should be(3)
      }
    }
  }
}
