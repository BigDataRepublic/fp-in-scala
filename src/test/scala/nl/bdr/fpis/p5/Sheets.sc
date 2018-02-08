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

