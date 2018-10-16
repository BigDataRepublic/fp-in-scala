package nl.bdr.fpis.generators

import org.scalacheck.{Arbitrary, Gen}

object ArbitraryStream {
  import nl.bdr.fpis.p5._

  implicit def arbStream[T](implicit a: Arbitrary[T]): Arbitrary[Stream[T]] =
    Arbitrary {

      def sizedStream(sz: Int): Gen[Stream[T]] =
        if (sz <= 0) Gen.const(Empty)
        else for {
          e <- Arbitrary.arbitrary[T]
          rest <- sizedStream(sz - 1)
        } yield Stream.cons(e, rest)

      Gen.sized(sz => sizedStream(sz))
    }
}
