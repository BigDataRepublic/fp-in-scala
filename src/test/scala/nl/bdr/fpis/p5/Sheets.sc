def conj(b1: Boolean, b2: Boolean) = b1 && b2

def negate(b: Boolean) = {
  println(s"Negating $b")
  !b
}

conj(negate(true), negate(false))

negate(true) && negate(false)

def conj_2(b1: Boolean, b2: => Boolean) = b1 && b2

conj_2(negate(true), negate(false))

def fillList[A](n: Int)(a: => A) = {
  val b = List.newBuilder[A]
  for (_ <- 0 until n) b += a
  b.result()
}

fillList(2)(negate(true))

def fillListLazy[A](n: Int)(a: => A) = {
  lazy val lazyA = a
  val b = List.newBuilder[A]
  for (_ <- 0 until n) b += lazyA
  b.result()
}

fillListLazy(2)(negate(true))

val (v1, v2, v3, v4) = (1, 2, 3, 4)

::(v1, ::(v2, ::(v3, ::(v4, Nil))))
v1 :: v2 :: v3 :: v4 :: Nil
v1 :: v2 :: v3 :: v4 :: Nil

List(v1, v2, v3, v4)

import nl.bdr.fpis.p5.Stream._
import nl.bdr.fpis.p5._

cons(v1, cons(v2, cons(v3, cons(v4, empty))))

Stream(v1, v2, v3, v4)

val str = cons(negate(true), cons(negate(true), cons(negate(true), empty)))
println(str.headOption)

ones.take(5).toList
List(1, 1, 1, 1, 1)



