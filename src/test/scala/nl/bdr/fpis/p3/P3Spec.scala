package nl.bdr.fpis.p3

import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.{Option => _}
import scala.{Either => _}

class P3Spec extends WordSpec with Matchers {

  "An option" when {

    import Option._

    "map" should {
      "bring None to None" in {
        None.map(_ => 1) should be(None)
      }
      "bring Option to another Option" in {
        Some(1).map(_ + 1) should be(Some(2))
      }
    }
    "flatmap" should {
      "bring a None to None" in {
        None.flatMap((a: Int) => Some(a + 1)) should be(None)
      }
      "bring an Option to another Option" in {
        Some(1).flatMap(a => Some(a + 1)) should be(Some(2))
      }
    }
    "getOrElse" should {
      "None should return the else" in {
        None.getOrElse(3) should be(3)
      }
      "option should return the original" in {
        Some(2).getOrElse(3) should be(2)
      }
    }
    "orElse" should {
      "None should return the else with None" in {
        None.orElse(Some(3)) should be(Some(3))
        None.orElse(None) should be(None)
      }
      "Some should return the original" in {
        Some(2).orElse(Some(3)) should be(Some(2))
      }
    }
    "filter" should {
      "None should be None" in {
        None.filter((x: Int) => x > 3) should be(None)
      }
      "Some should use the predicate" in {
        Some(2).filter((x: Int) => x > 3) should be(None)
        Some(5).filter((x: Int) => x > 3) should be(Some(5))
      }
    }
  }

  "An either" when {

    import Either._

    val left: Either[String, Int] = Left[String]("error")
    val right: Either[String, Int] = Right[Int](123)

    "mapping" should {
      "bring Left to the same" in {
        left.map(_ + 1) should be (left)
      }
      "bring Right to the function" in {
        right.map(_ + 1) should be (Right(124))
      }
    }
    "flatMap" should {
      "bring Left to the same" in {
        left.flatMap(i => Left("another " + i)) should be (left)
        left.flatMap(i => Right(i + 1)) should be (left)
      }
      "bring Right to the flatMapped value" in {
        right.flatMap(i => Left("wrong " + i)) should be (Left("wrong 123"))
        right.flatMap(i => Right(i + 10)) should be (Right(133))
      }
    }
    "orElse" should {
      "get a valid right value from a left" in {
        left.orElse(Right(12)) should be (Right(12))
        left.orElse(Left("oops")) should be (Left("oops"))
      }
      "get the same value from a right" in {
        right.orElse(Right(12)) should be (Right(123))
        right.orElse(Left("oops")) should be (Right(123))
      }
    }
    "map2" should {
      def add(a: Int, b: Int): Int = a + b

      "either1 is Left" in {
        left.map2(right)(add) should be (left)
      }
      "either2 is Left" in {
        right.map2(left)(add) should be (left)
      }
      "both are right" in {
        right.map2(right)(add) should be (Right(246))
      }
    }
  }
}
