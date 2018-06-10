package org.exercises.fpis.ch4

sealed trait C4Option[+A] {
  def map[B](f: A => B): C4Option[B] = this match {
    case C4Some(a) => C4Some(f(a))
    case C4None => C4None
  }
  def flatMap[B](f: A => C4Option[B]): C4Option[B] = map(f).getOrElse(C4None)
  def getOrElse[B >: A](default: => B): B = this match {
    case C4Some(a) => a
    case C4None => default
  }
  def orElse[B >: A](ob: => C4Option[B]): C4Option[B] = map(C4Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): C4Option[A] = map(f).flatMap(pred => if(pred) this else C4None)
}
case class C4Some[+A](get: A) extends C4Option[A]
case object C4None extends C4Option[Nothing]

