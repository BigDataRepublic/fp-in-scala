package nl.bdr.fpis.p2

import org.scalatest.Matchers
import org.scalatest.WordSpec

class ListSpec extends WordSpec with Matchers {

 "A list" when {

   import List._

   "empty" should {
     val emptyList = Nil

     "return an empty list on tail" in {
        tail(emptyList) should be(Nil)
     }

     "return a one element list on setHead" in {
       setHead(emptyList, 1) should be(List(1))
     }

     "drop nothing" when {
       "n < 0" should {
         val n = -1

         "return an empty list" in {
           drop(emptyList, n) should be(Nil)
         }
       }

       "n == 0" should {
         val n = 0
         "return an empty list" in {
           drop(emptyList, n) should be(Nil)
         }
       }

       "n > 0" should {
         val n = 1
         "return an empty list" in {
           drop(emptyList, n) should be(Nil)
         }
       }
     }
   }

   "having one element" should {
     val oneElementList = List(1)

     "return an emtpy list on tail" in {
       tail(oneElementList) should be(Nil)
     }

     "return a list with the head replaced on setHead" in {
       setHead(oneElementList, 2) should be(List(2))
     }

     "drop nothing" when {
       "n < 0" should {
         val n = -1

         "return an empty list" in {
           drop(oneElementList, n) should be(oneElementList)
         }
       }

       "n == 0" should {
         val n = 0
         "return an empty list" in {
           drop(oneElementList, n) should be(oneElementList)
         }
       }
     }

     "drop n elements" when {
       "n > 0" should {
         val n = 1
         "return an empty list" in {
           drop(oneElementList, n) should be(Nil)
         }
       }
     }
   }

   "having more than one element" should {
     val multiElementList = List(1, 2)

     "return an emtpy list on tail" in {
       tail(multiElementList) should be(List(2))
     }

     "return a list with the head replaced on setHead" in {
       setHead(multiElementList, 2) should be(List(2, 2))
     }
   }
 }

}
