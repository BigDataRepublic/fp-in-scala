package nl.bdr.fpis.generators

import nl.bdr.fpis.p2.{Branch, Leaf, Tree}
import org.scalacheck.{Arbitrary, Gen}

object ArbitraryTrees {

  implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] =
    Arbitrary {
      val genLeaf = for (e <- Arbitrary.arbitrary[T]) yield Leaf(e)

      def genBranch(sz: Int): Gen[Tree[T]] = for {
        left <- sizedTree(sz / 2)
        right <- sizedTree(sz / 2)
      } yield Branch(left, right)

      def sizedTree(sz: Int): Gen[Tree[T]] =
        if (sz <= 0) genLeaf
        else Gen.frequency((1, genLeaf), (3, genBranch(sz)))

      Gen.sized(sz => sizedTree(sz))
    }
}
