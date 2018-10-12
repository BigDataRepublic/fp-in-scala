package nl.bdr.fpis.generators

import org.scalacheck.{Arbitrary, Gen}

object ArbitraryList {
  import nl.bdr.fpis.p2.List
  import nl.bdr.fpis.p2.Nil
  import nl.bdr.fpis.p2.Cons

  implicit def arbList[T](implicit a: Arbitrary[T]): Arbitrary[List[T]] =
    Arbitrary {

      def sizedList(sz: Int): Gen[List[T]] =
        if (sz <= 0) Gen.const(Nil)
        else for {
          e <- Arbitrary.arbitrary[T]
          rest <- sizedList(sz - 1)
        } yield Cons(e, rest)

      Gen.sized(sz => sizedList(sz))
    }
}
