package nl.bdr.fpis.p3

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _}

// Exercise 4.1
//
// Implement all of the preceding functions on Option. As you implement each function,
// try to think about what it means and in what situations you’d use it. We’ll explore when
// to use each of these functions next. Here are a few hints for solving this exercise:
// * It’s fine to use pattern matching, though you should be able to implement all
//   the functions besides map and getOrElse without resorting to pattern matching.
// * For map and flatMap, the type signature should be enough to determine the
//   implementation.
// * getOrElse returns the result inside the Some case of the Option, or if the Option
//   is None, returns the given default value.
// * orElse returns the first Option if it’s defined; otherwise, it returns the second
//   Option.
trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = ???

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = ???

  override def getOrElse[B >: Nothing](default: => B): B = ???

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ???

  override def filter(f: Nothing => Boolean): Option[Nothing] = ???
}

case class Some[A](a: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = ???

  override def flatMap[B](f: A => Option[B]): Option[B] = ???

  override def getOrElse[B >: A](default: => B): B = ???

  override def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  override def filter(f: A => Boolean): Option[A] = ???
}

object HandlingErrors {
  import nl.bdr.fpis.p2.List
  import nl.bdr.fpis.p2.Nil
  import nl.bdr.fpis.p2.Cons


  // Section - 4.3
  /**
    * Optionally returns the mean of the sequence of doubles, or None when
    * the sequence is empty.
     */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)

  // Exercise - 4.2
  //
  // Implement the variance function in terms of flatMap. If the mean of a sequence is m ,
  // the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  // See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
  def variance(xs: Seq[Double]) : Option[Double] = ???

  // Exercise - 4.3
  //
  // Write a generic function map2 that combines two Option values using a binary function. If
  // either Option value is None, then the return value is too. Here is its signature:
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  // Exercise - 4.4
  //
  // Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some
  // with a list of all the values. Here is its signature:3
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  // Exercise - 4.5
  //
  // Implement the traverse function. It’s straightforward to do using map and sequence, but try
  // for a more efficient implementation that only looks at the list once. In fact, implement
  // sequence in terms of traverse .
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

}

