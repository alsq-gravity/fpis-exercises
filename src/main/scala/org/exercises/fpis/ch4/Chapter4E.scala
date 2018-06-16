package org.exercises.fpis.ch4

import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil}

import scala.annotation.tailrec

sealed trait C4Either[+E, +A] {
  // 4.6
  def map[B](f: A => B): C4Either[E,B] = this match {
    case C4Right(a) => C4Right(f(a))
    case l@C4Left(_) => l
  }
  def flatMap[EE >: E,B](f: A => C4Either[EE,B]): C4Either[EE, B] = this match {
    case C4Right(a) => f(a)
    case l@C4Left(_) => l
  }
  def orElse[EE >: E, B >: A](ob: => B): B =  this match {
    case C4Right(r) => r
    case C4Left(_) => ob
  }
}
case class C4Left[+E](v: E) extends C4Either[E, Nothing]
case class C4Right[+A](v: A) extends C4Either[Nothing, A]

object C4Either {

  import org.exercises.fpis.ch3.Chapter3L._

  def etry[EE >: Exception,A](a: => A): C4Either[EE,A] = try C4Right(a) catch { case e:Exception => C4Left(e) }
  // 4.6, cont.
  def map2[E,A,B,C](ea: C4Either[E,A], eb: C4Either[E,B])(f: (A,B) => C): C4Either[E,C] = ea.flatMap(a => eb.map(b => f(a,b)))

  // 4.7
  def traverse[E,A,B](la: C3List[A])(f: A => C4Either[E,B]): C4Either[E,C3List[B]] = {

    @tailrec
    def go(depleting: C3List[A], acc: C4Either[E,C3List[B]]): C4Either[E,C3List[B]] = depleting match {
      case C3Nil => acc
      case C3Cons(a,as) => acc match {
        case l@C4Left(_) => l
        case C4Right(aas) => f(a) match {
          case l@C4Left(_) => l
          case C4Right(r) => go(as,C4Right(C3Cons(r,aas)))
        }
      }
    }

    val resRev = go(la,C4Right(C3Nil))

    resRev match {
      case C4Left(_) => resRev
      case C4Right(ll) => C4Right(reverse(ll))
    }
  }
  def traverseR[E,A,B](la: C3List[A])(f: A => C4Either[E, B]): C4Either[E, C3List[B]] = la match {
    case C3Nil => C4Right(C3Nil)
    case C3Cons(a,as) => map2(f(a),traverse(as)(f))(C3Cons(_,_))
  }

  def sequence[E,A](lea: C3List[C4Either[E,A]]): C4Either[E, C3List[A]] = traverse(lea)(identity)

  def sequenceR[E,A](le: C3List[C4Either[E,A]]): C4Either[E, C3List[A]] = le match {
    case C3Nil => C4Right(C3Nil)
    case C3Cons(e,es) => map2(e,sequence(es))(C3Cons(_,_))
  }

}
