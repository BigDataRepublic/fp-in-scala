package nl.bdr.fpis.p5

import nl.bdr.fpis.generators.FPGeneratorConfiguration
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

object StreamSpec {
  val even: Int => Boolean = _ % 2 == 0
}

class StreamSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with FPGeneratorConfiguration {

  import Stream._
  import StreamSpec._
  import nl.bdr.fpis.generators.ArbitraryStream._

  def infiniteStream: Stream[Int] = Stream.cons(1, infiniteStream)

  "Ex 5.1 Stream.toList" should "convert streams to lists" in {
    Stream(1, 2, 3).toList should be(List(1, 2, 3))
    Stream.empty.toList should be(List())
  }

  "Ex. 5.2 Stream.take(n)" should "take the first elements from streams" in {
    Stream(1, 2, 3).take(2).toList should be(List(1, 2))
    Stream().take(2).toList should be(List())
    ones.take(5).toList should be(List(1, 1, 1, 1, 1))
  }

  "ex. 5.2 Stream.take(n)" should "have a similar behaviour as List" in {
    forAll("stream", "n") { (stream: Stream[Int], n: Int) =>
      stream.take(n).toList should be(stream.toList.take(n))
    }
  }


  "Ex. 5.2 Stream.drop(n)" should "drop the first elements from streams" in {
    Stream(1, 2, 3).drop(2).toList should be(List(3))
    Stream().drop(2).toList should be(List())
    infiniteStream.drop(2).take(2).toList should be(List(1, 1))
  }

  "Ex. 5.2 Stream.drop(n)" should "have a similar behaviour as list" in {
    forAll("stream", "n") { (stream: Stream[Int], n: Int) =>
      stream.drop(n).toList should be(stream.toList.drop(n))
    }
  }

  "Ex. 5.3 Stream.takeWhile(f)" should "take elements while the condition is true" in {
    Stream(2, 4, 5).takeWhile(even).toList should be(List(2, 4))
    Stream().takeWhile(even).toList should be(List())
    ones.takeWhile(even).toList should be(List())
  }

  "Ex. 5.4 Stream.forall(p)" should "correctly evaluate the proposition" in {
    Stream(2, 4, 5) forAll even should be(false)
    Stream(2, 4, 6) forAll even should be(true)
    Stream() forAll even should be(true)

    // Take an infinite stream to check for early termination
    ones.forAll(even) should be(false)
  }

  "Ex. 5.5 Stream.takeWhile2" should "take elements while the condition is true" in {
    Stream(2, 4, 5).takeWhile2(even).toList should be(List(2, 4))
    Stream().takeWhile2(even).toList should be(List())
    ones.takeWhile2(even).take(10).toList should be(List())
  }

  "Ex. 5.6 Stream.headOption" should "give the first element if the stream isn't empty" in {
    Stream().headOption2 should be(None)
    Stream(1).headOption2 should be(Some(1))
    ones.headOption2 should be(Some(1))
  }

  "Ex. 5.7 Stream.map" should "correctly map over a stream" in {
    Stream().map(identity).toList should be(List())
    Stream(2, 3, 4).map(_ * 2).toList should be(List(4, 6, 8))
    ones.map(_ * 2).take(2).toList should be(List(2, 2))
  }

  "Ex. 5.7 Stream.filter" should "correctly filter a stream" in {
    //ones.filter(_ == 2).take(2).toList should be (List(2, 2))
    Stream(2, 3, 4).filter(_ < 4).toList should be(List(2, 3))
    Stream[Int]().filter(_ < 4).toList should be(List())
  }

  "Ex. 5.7 Stream.append" should "correctly append two streams" in {
    val s1 = Stream(2, 3, 4)
    val s2 = Stream(10, 11, 12)
    s1.append(s2).toList should be(List(2, 3, 4, 10, 11, 12))
    s1.append(Empty).toList should be(List(2, 3, 4))
    Empty.append(s1).toList should be(List(2, 3, 4))
    Empty.append(Empty) should be(Empty)

    // Append an infinite list to check for non-strictness
    s1.append(ones).take(5).toList should be(List(2, 3, 4, 1, 1))
  }

  "Ex. 5.7 Stream.flatMap" should "correctly flatmap a stream" in {
    val s1 = Stream(2, 3, 4)
    s1.flatMap(s => Stream(s * 2, s * 3, s * 10)).toList should be(
      List(4, 6, 20, 6, 9, 30, 8, 12, 40)
    )
  }
  "Ex. 5.7 Stream.flatMap" should "Monad laws: Left identity: Stream(x) flatMap f == f(x)" in {
    forAll("x", "f") { (x: Int, f: Int => Stream[Int]) =>
      Stream(x).flatMap(f) should be(f(x))
    }
  }

  "Ex. 5.7 Stream.flatMap" should "Monad laws: Right identity: (m flatMap Stream(_) === m" in {
    forAll("m") { m: Stream[Int] =>
      m.flatMap(Stream(_)) should be(m)
    }
  }

  "Ex. 5.7 Stream.flatMap" should "Monad laws: Associativity: (m flatMap f) flatMap g === m flatMap { x => f(x) flatMap g }" in {
    forAll("m", "f", "g") { (m: Stream[Int], f: Int => Stream[Int], g: Int => Stream[Int]) =>
      m.flatMap(f).flatMap(g) should be(m.flatMap { x => f(x).flatMap(g) })
    }
  }

  "Ex. 5.8 Stream.constant" should "create a constant stream" in {
    constant(None).take(3).toList should be(List(None, None, None))
  }

  "Ex. 5.9 Stream.from" should "create a increasing stream" in {
    from(5).take(5).toList should be(List(5, 6, 7, 8, 9))
  }

  "Ex. 5.10 Stream.fibs" should "create a fibonaci sequence" in {
    fibs.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  "Ex. 5.12 Stream.fibs2" should "create a fibonaci sequence as well" in {
    fibs2.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  "Ex. 5.12 Stream.constant2" should "create a constant stream" in {
    ones2.take(3).toList should be(List(1, 1, 1))
  }

  "Ex. 5.12 Stream.from2" should "create a increasing stream" in {
    from2(5).take(5).toList should be(List(5, 6, 7, 8, 9))
  }

  "Ex. 5.13 Stream.takeWhile3" should "take elements while the condition is true" in {
    Stream(2, 4, 5).takeWhile3(even).toList should be(List(2, 4))
    Stream().takeWhile3(even).toList should be(List())
    ones.takeWhile3(even).toList should be(List())
  }

  "Ex. 5.13 Stream.zipWith" should "zip two streams" in {
    Stream("one", "two", "three").zipWith(Stream(1, 2, 3)).toList should be(
      List(("one", 1), ("two", 2), ("three", 3)))

    ones.zipWith(ones).take(5).toList should be(Seq.fill(5)(1, 1))
    Empty.zipWith(Empty) should be(Empty)
  }

  "Ex. 5.13 Stream.zipAll" should "zip two streams with options" in {
    Stream(1).zipAll(Stream(1, 2, 3)).toList should be(
      List((Some(1), Some(1)), (None, Some(2)), (None, Some(3))))
  }

  "Ex. 5.14 Stream.startsWith" should "detect starts of streams" in {

    // Test positive cases
    forAll("s1", "s2") { (s1: Stream[Int], s2: Stream[Int]) =>
      s1.append(s2).startsWith(s1) should be (true)
    }
    // Test negative cases
    forAll("s1", "s2", "s3") { (s1: Stream[Int], s2: Stream[Int], s3: Stream[Int]) =>
      whenever (s1 != s2) {
        s1.append(s3).startsWith(s2) should be (false)
      }
    }
    // Test infinite cases
    ones.startsWith(Stream(1,1,1)) should be (true)
    ones.startsWith(Stream(1,1,2)) should be (false)
    Empty.startsWith(Stream(1)) should be (false)
    Empty.startsWith(Empty) should be (true)
  }

  "Ex. 5.15 Stream.tails" should "list all tails" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) should be(
      List(List(1, 2, 3), List(2, 3), List(3)))
  }

  "Ex. extra" should "do Newton-Rapson approximation" in {
    // Approximate pi in 3 newtonRapson iterations
    val piApprox = newton(3, Math.sin, Math.cos).drop(3).headOption
    piApprox should be (Some(Math.PI))
  }

}
