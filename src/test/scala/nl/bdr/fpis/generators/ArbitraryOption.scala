package nl.bdr.fpis.generators

import nl.bdr.fpis.p3.{None, Option, Some}
import org.scalacheck.{Arbitrary, Gen}
import scala.{Either => _}

object ArbitraryOption {

  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary {
      val genSome = for (e <- Arbitrary.arbitrary[T]) yield Some(e)
      Gen.oneOf(Gen.const(None), genSome)
    }
}


