package nl.bdr.fpis.p1

object Chapter2 {
  // Exercise 2.1
  //
  // Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
  // The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
  // previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
  // local tail-recursive function.
  def fib(n: Int): Int = ???


  // Exercise 2.2
  //
  //  Implement isSorted, which checks whether an Array[A] is sorted according to a
  //  given comparison function:
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = ???

  //noinspection ScalaUnnecessaryParentheses
  // Exercise 2.3
  //
  //  Let’s look at another example, currying,9 which converts a function f of two arguments
  //  into a function of one argument that partially applies f. Here again there’s only one
  //  implementation that compiles. Write this implementation.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = ???

  // Exercise 2.4
  //
  //  Implement uncurry , which reverses the transformation of curry . Note that since =>
  //  associates to the right, A => (B => C) can be written as A => B => C.
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = ???


  // Exercise 2.5
  //
  //  Implement the higher-order function that composes two functions.
  def compose[A,B,C](f: B => C, g: A => B): A => C = ???

}
