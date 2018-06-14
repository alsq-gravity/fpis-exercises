package org.exercises.fpis.ch4

sealed trait C4Either[+E, +A] {
  def map[B](f: A => B): C4Either[E,B] = ???
  def flatMap[B](f: A => C4Option[B]): C4Either[E, B] = ???
  def orElse[EE >: E, B >: A](ob: => C4Either[EE, B]): C4Either[EE, B] = ???
}
case class C4Left[+E](v: E) extends C4Either[E, Nothing]
case class C4Right[+A](v: A) extends C4Either[Nothing, A]

object C4Either {

  def map2[E,A,B,C](ea: C4Either[E,A], eb: C4Either[E,B])(f: (A,B) => C): C4Either[E,C] = ???
}
