package nl.bdr.fpis.p3

import nl.bdr.fpis.p2
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.{Option => _}
import scala.{Either => _}
import scala.{Left => _}
import scala.{Right => _}

class P3Spec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  "An option" when {

    import nl.bdr.fpis.generators.ArbitraryOption._

    "map" should {
      "bring None to None" in {
        None.map(_ => 1) should be(None)
      }
      "bring Option to another Option" in {
        Some(1).map(_ + 1) should be(Some(2))
      }
      "Functor laws: identity" in {
        forAll("o") { t1: Option[Unit] => t1.map(identity) should be(t1) }
      }
      "Functor laws: associativity" in {
        forAll("f1", "f2", "o") { (f1: Int => Int, f2: Int => Int, o: Option[Int]) =>
          o.map(f1).map(f2) should be(o.map(f2.compose(f1)))
        }
      }
    }
    "flatmap" should {
      "bring a None to None" in {
        None.flatMap((a: Int) => Some(a + 1)) should be(None)
      }
      "bring an Option to another Option" in {
        Some(1).flatMap(a => Some(a + 1)) should be(Some(2))
      }
      "Monad laws: Left identity: Some(x) flatMap f == f(x)" in {
        forAll("x", "f") { (x: Int, f: Int => Option[Int]) =>
          Some(x).flatMap(f) should be(f(x))
        }
      }
      "Monad laws: Right identity: (m flatMap Some(_) === m" in {
        forAll("m") { m: Option[Int] =>
          m.flatMap(Some(_)) should be(m)
        }
      }
      "Monad laws: Associativity: (m flatMap f) flatMap g === m flatMap { x => f(x) flatMap g }" in {
        forAll("m", "f", "g") { (m: Option[Int], f: Int => Option[Int], g: Int => Option[Int]) =>
          m.flatMap(f).flatMap(g) should be(m.flatMap { x => f(x).flatMap(g) })
        }
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

  "Variance calculation" should {
    import HandlingErrors._
    "return None when given an empty Seq" in {
      variance(Seq()) should be(None)
    }
    "return zero when given any single value" in {
      forAll("v") { v: Double => variance(Seq(v)) should be(Some(0)) }
    }
    "return the absolute difference when given two values" in {
      // For the readers: this allows us to compare two doubles with a certain tolerance. Otherwise
      // the automatic test generator will find cases where the two calculations don't exactly match.
      implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-200)
      forAll("v1", "v2") { (v1: Double, v2: Double) =>
        variance(Seq(v1, v2)).getOrElse(-1) === Math.abs(v1 - v2)
      }
    }
    "just a simple test" in {
      variance(Seq(1, 3, 5, 7, 9)) should be(Some(8))
    }
  }

  "An either" when {

    import nl.bdr.fpis.generators.ArbitraryEither._

    val left: Either[String, Int] = Left[String]("error")
    val right: Either[String, Int] = Right[Int](123)

    "mapping" should {
      "bring Left to the same" in {
        left.map(_ + 1) should be(left)
      }
      "bring Right to the function" in {
        right.map(_ + 1) should be(Right(124))
      }
      "functor laws: identity" in {
        forAll("e") { e: Either[String, Int] =>
          e.map(identity) should be(e)
        }
      }
      "functor laws: associativity" in {
        forAll("f1", "f2", "e") { (f1: Int => Int, f2: Int => Int, e: Either[String, Int]) =>
          e.map(f1).map(f2) should be(e.map(f2.compose(f1)))
        }
      }
    }
    "flatMap" should {
      "bring Left to the same" in {
        left.flatMap(i => Left("another " + i)) should be(left)
        left.flatMap(i => Right(i + 1)) should be(left)
      }
      "bring Right to the flatMapped value" in {
        right.flatMap(i => Left("wrong " + i)) should be(Left("wrong 123"))
        right.flatMap(i => Right(i + 10)) should be(Right(133))
      }
      "Monad laws: Left identity: Some(x) flatMap f == f(x)" in {
        forAll("x", "f") { (x: Int, f: Int => Either[String, Int]) =>
          Right(x).flatMap(f) should be(f(x))
        }
      }
      "Monad laws: Right identity: (m flatMap Some(_) === m" in {
        forAll("m") { m: Either[String, Int] =>
          m.flatMap(Right(_)) should be(m)
        }
      }
      "Monad laws: Associativity: (m flatMap f) flatMap g === m flatMap { x => f(x) flatMap g }" in {
        forAll("m", "f", "g") { (m: Either[String, Int], f: Int => Either[String, Int], g: Int => Either[String, Int]) =>
          m.flatMap(f).flatMap(g) should be(m.flatMap { x => f(x).flatMap(g) })
        }
      }
    }
    "orElse" should {
      "get a valid right value from a left" in {
        left.orElse(Right(12)) should be(Right(12))
        left.orElse(Left("oops")) should be(Left("oops"))
      }
      "get the same value from a right" in {
        right.orElse(Right(12)) should be(Right(123))
        right.orElse(Left("oops")) should be(Right(123))
      }
    }
    "map2" should {
      def add(a: Int, b: Int): Int = a + b

      "either1 is Left" in {
        left.map2(right)(add) should be(left)
      }
      "either2 is Left" in {
        right.map2(left)(add) should be(left)
      }
      "both are right" in {
        right.map2(right)(add) should be(Right(246))
      }
    }
    "traverse" should {
      import nl.bdr.fpis.p2.List
      import nl.bdr.fpis.generators.ArbitraryList._
      import Either._

      "normal test to left" in {
        sequence(List(Right(123), Left("error"), Right(345))) should be(Left("error"))
      }
      "normal test to right" in {
        sequence(List(Right(123), Right(345))) should be(Right(List(123, 345)))
      }
      "identity (traverse Identity = Identity)" in {
        forAll("l") { l: List[Int] =>
          sequence[String, Int](List.map(l)(Right(_))) should be(Right(l))
        }
      }
      "composition (traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f)" in {
        forAll("l", "f", "g") { (l: List[Byte], f: Byte => Either[Byte, String], g: String => Either[Byte, Int]) =>
          // dist · fmap Comp = Comp · fmap dist · dist
          val fg = f.andThen(_.flatMap(g))
          traverse(l)(fg).orElse(Left(-1)) should be(traverse(l)(f).flatMap(l2 => traverse(l2)(g)).orElse(Left(-1)))
        }
      }
    }
  }
}
