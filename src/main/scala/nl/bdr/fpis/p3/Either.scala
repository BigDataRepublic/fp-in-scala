package nl.bdr.fpis.p3

// Exercise - 4.6
//
// Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
// When mapping over the right side, we must promote the left type parameter to some supertype, to
// satisfy the +E variance annotation.
trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

object Either {
  // Exercise - 4.7
  //
  // Implement sequence and traverse for Either . These should return the first error that’s
  // encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???
}
