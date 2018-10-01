package nl.bdr.fpis.p2

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.scalatest.{Matchers, WordSpec}

object TreeSpecification {

  implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] =
    Arbitrary {
      val genLeaf = for (e <- Arbitrary.arbitrary[T]) yield Leaf(e)

      def genInternal(sz: Int): Gen[Tree[T]] = for {
        left <- sizedTree(sz / 2)
        right <- sizedTree(sz / 2)
      } yield Branch(left, right)

      def sizedTree(sz: Int): Gen[Tree[T]] =
        if (sz <= 0) genLeaf
        else Gen.frequency((1, genLeaf), (3, genInternal(sz)))

      Gen.sized(sz => sizedTree(sz))
    }
}

class TreeSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks{
  import TreeSpecification._


  "A tree" when {

    import Tree._

    "the size" should {
      "combining two trees as one, the size of both trees plus one should be the size of the resulting tree minus one" in {
        forAll("t1", "t2") { (t1: Tree[Unit], t2: Tree[Unit]) =>
          Tree.size(t1) + Tree.size(t2) should be(Tree.size(Branch(t1, t2)) - 1)
        }
      }
      "size of a single leaf should be 1" in {
        Tree.size(Leaf(0)) should be(1)
      }
    }

    "the max" should {
      "the max of two trees combined should be the max of the max of both trees" in {
        forAll("t1", "t2") { (t1: Tree[Int], t2: Tree[Int]) =>
          Math.max(maximum(t1), maximum(t2)) should be(maximum(Branch(t1, t2)))
        }
      }
      "the max of a single leaf should be that leaf's value" in {
        forAll("v") { v: Int => maximum(Leaf(v)) should be(v) }
      }
    }

    "the depth" should {
      "the depth of two trees combined should be the max of the depth of both subtrees, minus one" in {
        forAll("t1", "t2") { (t1: Tree[Unit], t2: Tree[Unit]) =>
          Math.max(depth(t1), depth(t2)) should be(depth(Branch(t1, t2)) - 1)
        }
      }
      "the depth of a single leaf should be 1" in {
        depth(Leaf(0)) should be(1)
      }
    }

    "map" should {
      val f: String => Int = el => el.length
      "mapping of two trees combined should be the result of combining both trees and mapping over it" in {
        forAll("t1", "t2") { (t1: Tree[String], t2: Tree[String]) =>
          Branch(map(t1)(f), map(t2)(f)) should be(map(Branch(t1, t2))(f))
        }
      }
      "not change the structure of the tree" in {
        forAll("t") { t: Tree[String] =>
          depth(t) should be(depth(map(t)(f)))
          Tree.size(t) should be(Tree.size(map(t)(f)))
        }
      }
    }
  }
}
