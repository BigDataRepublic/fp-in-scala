package nl.bdr.fpis.p3

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _}

// Exercise - 4.6
//
// Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
// When mapping over the right side, we must promote the left type parameter to some supertype, to
// satisfy the +E variance annotation.
//
// Think about how to represent Left and Right before implementing the functions below.
trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}

object Either {
  // Exercise - 4.7
  //
  // Implement sequence and traverse for Either . These should return the first error that’s
  // encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  // Exercise - 4.8
  //
  //  As a final example, here’s an application of map2, where the function mkPerson validates both
  // the given name and the given age before constructing a valid Person.

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  //  In this implementation, map2 is only able to report one error, even if both the name
  //  and the age are invalid. What would you need to change in order to report both errors?
  //  Would you change map2 or the signature of mkPerson? Or could you create a new data
  //  type that captures this requirement better than Either does, with some additional
  //  structure? How would orElse, traverse, and sequence behave differently for that
  //  data type?
}
