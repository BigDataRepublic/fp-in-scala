package nl.bdr.fpis.p2

import nl.bdr.fpis.generators.FPGeneratorConfiguration
import org.scalatest.Matchers
import org.scalactic.{Equality, Equivalence}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.{Await, ExecutionContext, Future}

class ListSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks with FPGeneratorConfiguration {
  import nl.bdr.fpis.generators.ArbitraryList._
  import List._

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200, sizeRange = 10)

  "A list" when {
    "empty" should {
      val emptyList = Nil

      "return an empty list on tail" in {
        tail(emptyList) should be(Nil)
      }

      "return a one element list on setHead" in {
        setHead(emptyList, 1) should be(List(1))
      }

      "drop nothing" when {
        "n < 0" should {
          val n = -1

          "return an empty list" in {
            drop(emptyList, n) should be(Nil)
          }
        }

        "n == 0" should {
          val n = 0
          "return an empty list" in {
            drop(emptyList, n) should be(Nil)
          }
        }

        "n > 0" should {
          val n = 1
          "return an empty list" in {
            drop(emptyList, n) should be(Nil)
          }
        }
      }
    }

    "having one element" should {
      val oneElementList = List(1)

      "return an emtpy list on tail" in {
        tail(oneElementList) should be(Nil)
      }

      "return a list with the head replaced on setHead" in {
        setHead(oneElementList, 2) should be(List(2))
      }

      "drop nothing" when {
        "n < 0" should {
          val n = -1

          "return an empty list" in {
            drop(oneElementList, n) should be(oneElementList)
          }
        }

        "n == 0" should {
          val n = 0
          "return an empty list" in {
            drop(oneElementList, n) should be(oneElementList)
          }
        }
      }

      "drop n elements" when {
        "n > 0" should {
          val n = 1
          "return an empty list" in {
            drop(oneElementList, n) should be(Nil)
          }
        }
      }
    }

    "having more than one element" should {
      val multiElementList = List(1, 2)

      "return an emtpy list on tail" in {
        tail(multiElementList) should be(List(2))
      }

      "return a list with the head replaced on setHead" in {
        setHead(multiElementList, 2) should be(List(2, 2))
      }
    }

    "sumFL" should {
      "empty list should sum to 0" in (List.sumFL(Nil) should be (0))
      "d + sum(l1) should be the same as sum(Cons(d, l1))" in {
        forAll ("l1", "d") { (l1: List[Double], d: Double) =>
          d + List.sumFL(l1) should be (List.sumFL(Cons(d, l1)))
        }
      }
    }

    "length" should {
      "empty list should have length 0" in (List.length(Nil) should be (0))
      "1 + length(l1) should be the same as length(Cons(d, l1))" in {
        forAll ("l1", "l2") { (l1: List[Int], d: Int) =>
          1 + List.length(l1) should be (List.length(Cons(d, l1)))
        }
      }
    }

    "reverse" should {
      "empty reverse should be Nil" in (List.reverse(Nil) should be (Nil))
      "reverse of reverse should be the same list" in {
        forAll("l1") { l1: List[Int] => List.reverse(List.reverse(l1)) should be (l1) }
      }
      "for a list a and b, append(reverse(l1), reverse(l2)) should be reverse(append(l2, l1))" in {
         forAll("l1", "l2") { (l1: List[Int], l2: List[Int]) =>
           List.append(List.reverse(l1), List.reverse(l2)) should be (List.reverse(List.append(l2, l1)))
         }
      }
    }

    "append" should {
      "for a list a and b, appendFL(l1, l2) should be append(l1, l2))" in {
        forAll("l1", "l2") { (l1: List[Int], l2: List[Int]) =>
          List.appendFL(l1, l2) should be (List.append(l1, l2))
        }
      }
    }

    "flatmap" should {
      "Monad laws: Left identity: List(x) flatMap f == f(x)" in {
        forAll("x", "f") { (x: Int, f: Int => List[Int]) =>
          List.flatMap(Cons(x, Nil))(f) should be(f(x))
        }
      }
      "Monad laws: Right identity: (m flatMap List(_) === m" in {
        forAll("m") { m: List[Int] =>
          List.flatMap(m)(Cons(_, Nil)) should be(m)
        }
      }
      "Monad laws: Associativity: (m flatMap f) flatMap g === m flatMap { x => f(x) flatMap g }" in {
        forAll("m", "f", "g") { (m: List[Int], f: Int => List[Int], g: Int => List[Int]) =>
          flatMap(flatMap(m)(f))(g) should be(flatMap(m) { x => flatMap(f(x))(g) })
        }
      }
    }
  }
}
