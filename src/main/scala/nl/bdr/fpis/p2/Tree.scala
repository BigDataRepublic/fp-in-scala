package nl.bdr.fpis.p2

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  //
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size(t: Tree[_]): Int = ???

  // Exercise 3.26
  //
  // Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you
  // can use x.max(y) or x max y to compute the maximum of two integers x and y.)
  def maximum[A](t: Tree[A])(implicit o: Ordering[A]): A = ???

  // Exercise 3.27
  //
  // Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  def depth(t: Tree[_]): Int = ???

  // Exercise 3.28
  //
  // Write a function map, analogous to the method of the same name on List, that modifies each
  // element in a tree with a given function.
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = ???
}
