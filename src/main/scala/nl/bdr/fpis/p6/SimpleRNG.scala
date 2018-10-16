package nl.bdr.fpis.p6


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  // Ex. 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  // Ex. 6.2
  def double(rng: RNG): (Double, RNG) = ???

  // Ex. 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  // Ex. 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  // Ex. 6.3
  def double3(rng1: RNG): ((Double, Double, Double), RNG) = ???

  // Ex. 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

}

// Ch. 6.4
object RNGApi {
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(RNG.nonNegativeInt)(i => i - i % 2)

  // Ex. 6.5
  def double: Rand[Double] = ???

  // Ex. 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  // Ch. 6.4.1
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Ex. 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  // Ex. 6.7
  def ints(count: Int): Rand[List[Int]] = ???

  // Ex. 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  // Ex. 6.9 (map using flatMap)
  def map_6_9[A, B](s: Rand[A])(f: A => B): Rand[B] = ???

  def map2_6_9[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
