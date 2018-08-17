package nl.bdr.fpis.p3

import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.{Option => _}

class P3Spec extends WordSpec with Matchers {

  "A list" when {

    import Option._

    "Option" should {
      "map a None to None" in {
        None.map(_ => 1) should be(None)
      }
      "map an Option to another Option" in {
        Some(1).map(_ + 1) should be(Some(2))
      }
      "flatMap a None to None" in {
        None.flatMap((a: Int) => Some(a + 1)) should be(None)
      }
      "flatMap an Option to another Option" in {
        Some(1).flatMap(a => Some(a + 1)) should be(Some(2))
      }
      "getOrElse of a None should return the else" in {
        None.getOrElse(3) should be(3)
      }
      "getOrElse of an option should return the original" in {
        Some(2).getOrElse(3) should be(2)
      }
      "orElse of a None should return the else with None" in {
        None.getOrElse(Some(3)) should be (Some(3))
        None.getOrElse(None) should be (None)
      }
      "orElse of a Some should return the original" in {
        Some(2).orElse(Some(3)) should be (Some(2))
      }
      "filter of None should be None" in {
        None.filter( (x: Int) => x > 3) should be (None)
      }
      "filter of Some should filter the value" in {
        Some(2).filter( (x: Int) => x > 3) should be (None)
        Some(5).filter( (x: Int) => x > 3) should be (Some(5))
      }
    }
  }
}
