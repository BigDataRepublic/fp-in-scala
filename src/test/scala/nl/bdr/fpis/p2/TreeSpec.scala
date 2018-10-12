package nl.bdr.fpis.p2

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}


class TreeSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  import nl.bdr.fpis.generators.ArbitraryTrees._

  "A tree" when {

    import Tree._

    "the size" should {
      "combining two trees as one, adding the size of both trees should equal the size of the combined tree minus one" in {
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
      val f: Int => String = el => el.toString
      "mapping of two trees combined should be the result of combining both trees and mapping over it" in {
        forAll("t1", "t2") { (t1: Tree[Int], t2: Tree[Int]) =>
          Branch(map(t1)(f), map(t2)(f)) should be(map(Branch(t1, t2))(f))
        }
      }
      "not change the structure of the tree" in {
        forAll("t") { t: Tree[Int] =>
          map(t)(identity) should be(t)
          depth(t) should be(depth(map(t)(f)))
          Tree.size(t) should be(Tree.size(map(t)(f)))
        }
      }
    }
  }
}
