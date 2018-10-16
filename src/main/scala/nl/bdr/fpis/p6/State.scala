package nl.bdr.fpis.p6

case class State[S, +A](run: S => (A, S)) {

  // Ex. 6.10
  def flatMap[B](g: A => State[S, B]): State[S, B] = ???

  // Ex. 6.10
  def map[B](f: A => B): State[S, B] = ???

  // Ex. 6.10
  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = ???
}

object State {

  // Ex. 6.10
  def unit[S, A](a: A): State[S, A] = ???

  // Ex. 6.10
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = ???

  // Chapter 6.6
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

object RandAsState {
  type Rand[A] = State[RNG, A]
}
