package nl.bdr.fpis.p5

sealed trait Stream[+A] {

  // Text 5.2
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Ex. 5.1
  /**
    * Force the evaluation of the stream, and return the stream as a list.
    */
  def toList: List[A] = ???

  // Ex. 5.2
  /**
    * Take the first 'n' elements of the stream and return a new stream.
    */
  def take(n: Int): Stream[A] = ???

  // Ex. 5.2
  /**
    * Drop the first 'n' elements of the stream and return a new stream.
    */
  def drop(n: Int): Stream[A] = ???

  // Ex. 5.3
  /**
    * Takes the elements from the stream as long as the condition is true.
    */
  def takeWhile(p: A => Boolean): Stream[A] = ???

  // Text 5.3
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Text 5.3
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsWithFold(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // Ex. 5.4
  def forAll(p: A => Boolean): Boolean = ???

  // Ex. 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = ???

  // Ex. 5.6
  def headOption2: Option[A] = ???

  // Ex. 5.7
  def map[B](f: A => B): Stream[B] = ???

  // Ex. 5.7
  def filter(f: A => Boolean): Stream[A] = ???

  // Ex. 5.7
  def append[B >: A](other: Stream[B]): Stream[B] = ???

  // Ex. 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  //map, take, takeWhile, zipWith
  // Ex. 5.13
  def map2[B](f: A => B): Stream[B] = ???

  // Ex. 5.13
  def take2(n: Int): Stream[A] = ???

  // Ex. 5.13
  def takeWhile3(f: A => Boolean): Stream[A] = ???

  // Ex. 5.13
  def zipWith[B](s2: Stream[B]): Stream[(A, B)] = ???

  // Ex. 5.13
  // Probably better way to write this
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

  // Ex. 5.14
  def startsWith[B >: A](stream: Stream[B]): Boolean = ???

  // Ex. 5.15
  def tails: Stream[Stream[A]] = ???

  // Text p.77
  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons[A](as.head, apply[A](as.tail: _*))

  // Text 5.4
  def ones: Stream[Int] = cons(1, ones)

  // Ex. 5.8
  def constant[A](a: A): Stream[A] = ???

  // Ex. 5.9
  def from(n: Int): Stream[Int] = ???

  // Ex. 5.10
  def fibs: Stream[Int] = ???

  // Ex. 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  // Ex. 5.12
  def fibs2: Stream[Int] = ???

  // Ex. 5.12
  def from2(n: Int): Stream[Int] = ???

  // Ex. 5.12
  def ones2: Stream[Int] = ???
}


