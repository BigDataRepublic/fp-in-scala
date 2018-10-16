package nl.bdr.fpis.p6

import nl.bdr.fpis.generators.FPGeneratorConfiguration
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with FPGeneratorConfiguration {

  import Ordering._
  import nl.bdr.fpis.generators.ArbitraryRNG._

  "ex. 6.1 RNG.nonNegativeInt(rng)" should "provide positive results with each RNG and a new RNG" in {
    forAll("rng") { rng: RNG =>
      val (nextInt, nextRng) = RNG.nonNegativeInt(rng)
      nextInt should be >= 0
      nextRng shouldNot be(rng)
      val (nextNextInt, _) = RNG.nonNegativeInt(nextRng)
      nextNextInt shouldNot be(nextInt)
    }
  }

  "ex. 6.2 RNG.double(rng)" should "provide a result between 0 and 1 with each RNG and a new RNG" in {
    forAll("rng") { rng: RNG =>
      val (nextDouble, nextRng) = RNG.double(rng)
      nextDouble should be >= 0d
      nextDouble should be <= 1d
      nextRng shouldNot be(rng)
      val (nextNextDouble, _) = RNG.double(nextRng)
      nextNextDouble shouldNot be(nextDouble)
    }
  }

  "ex. 6.3 intDouble(rng)" should "produce correct pairs and a correct new RNG" in {
    forAll("rng") { rng: RNG =>
      val ((_, _), rng2) = RNG.intDouble(rng)
      rng2 shouldNot be(rng)
      val (_, rng3) = rng.nextInt
      rng3 shouldNot be(rng)
    }
  }

  "ex. 6.4 ints(count)(rng)" should "produce a correct set of ints" in {
    forAll(arbitraryRng.arbitrary, Gen.choose(1, 1000)) { (rng: RNG, count: Int) =>
      val (list, rng2) = RNG.ints(count)(rng)
      list.size shouldBe count
      rng2 shouldNot be(rng)
    }
  }

  "ex. 6.5 RNG.double(rng)" should "provide a result between 0 and 1 with each RNG and a new RNG" in {
    import RNGApi._

    forAll("rng") { rng: RNG =>
      val (nextDouble, nextRng) = double(rng)
      nextDouble should be >= 0d
      nextDouble should be <= 1d
      nextRng shouldNot be(rng)
      val (nextNextDouble, _) = double(nextRng)
      nextNextDouble shouldNot be(nextDouble)
    }
  }

  "ex. 6.6 map2(Rand, Rand)" should "combine two Rands into one" in {
    import RNGApi._
    forAll("rng") { rng: RNG =>
      val ((a, b), rng2) = both(RNGApi.int, int)(rng)
      val (a2, rng3) = int(rng)
      a should be(a2)
      b should be(int(rng3)._1)
      rng2 shouldNot be(rng)
    }
  }

  "ex. 6.7 sequence(listOfRand)" should "correctly sequence a list of rands" in {
    import RNGApi._

    forAll("rng") { rng: RNG =>
      val seqRand: Rand[List[Int]] = sequence(List(int, nonNegativeEven, unit(3)))
      val (list, rng2) = seqRand(rng)
      rng2 shouldNot be(rng)
      list(1) should be >= 0
      list(2) should be(3)
    }
  }

  "Ex. 6.8 Rand.flatMap" should "Monad laws: Left identity: Rand(x) flatMap f == f(x)" in {
    import RNGApi._

    forAll("x", "f", "rng") { (x: Int, f: Int => Rand[Int], rng: RNG) =>
      // Should apply with rng, since we cannot compare functions
      flatMap(unit(x))(f)(rng) should be(f(x)(rng))
    }
  }

  "Ex. 6.8 Rand.flatMap" should "Monad laws: Right identity: (m flatMap Rand(_) === m" in {
    import RNGApi._

    forAll("m", "rng") { (m: Rand[Int], rng: RNG) =>
      // Should apply with rng, since we cannot compare functions
      flatMap(m)(unit)(rng) should be(m(rng))
    }
  }

  "Ex. 6.8 Rand.flatMap" should "Monad laws: Associativity: (m flatMap f) flatMap g === m flatMap { x => f(x) flatMap g }" in {
    import RNGApi._

    forAll("m", "f", "g", "rng") {
      (m: Rand[Int], f: Int => Rand[Int], g: Int => Rand[Int], rng: RNG) =>
        // Should apply with rng, since we cannot compare functions
        flatMap(flatMap(m)(f))(g)(rng) should be(flatMap(m) { x => flatMap(f(x))(g) }(rng))
    }
  }

}
