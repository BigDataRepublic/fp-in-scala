package nl.bdr.fpis.generators

import nl.bdr.fpis.p6.{RNG, SimpleRNG}
import org.scalacheck.{Arbitrary, Cogen, Gen}

object ArbitraryRNG {

  implicit val genRng: Gen[RNG] = Arbitrary.arbitrary[Long].map(SimpleRNG)

  implicit val cogenRng: Cogen[RNG] = Cogen.cogenLong.contramap((rng: SimpleRNG) => rng.seed).asInstanceOf[Cogen[RNG]]

  implicit val arbitraryRng: Arbitrary[RNG] = Arbitrary {
    Arbitrary.arbitrary[Long].map(SimpleRNG)
  }

}
