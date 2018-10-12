package nl.bdr.fpis.generators

import org.scalacheck.{Arbitrary, Gen}
import scala.{Either => _}
import Arbitrary._
import Gen._

object ArbitraryEither {
  import nl.bdr.fpis.p3.Either
  import nl.bdr.fpis.p3.Left
  import nl.bdr.fpis.p3.Right

  implicit def arbEither[L, R](implicit a: Arbitrary[L], b: Arbitrary[R]): Arbitrary[Either[L, R]] =
    Arbitrary(oneOf(arbitrary[L].map(Left(_)), arbitrary[R].map(Right(_))))

  implicit def arbRight[R](implicit b: Arbitrary[R]): Arbitrary[Right[R]] =
    Arbitrary(arbitrary[R].map(Right(_)))
}
