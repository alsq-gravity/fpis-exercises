package org.exercises.fpis.ch5

import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil, Chapter3L}

import scala.annotation.tailrec

sealed trait C5Stream[+A] {

  // 5.1
  def toList: C3List[A] = {
    @tailrec
    def go(depleting: C5Stream[A], acc: C3List[A]): C3List[A] = depleting match {
      case C5Empty => acc
      case C5Cons(at, ats) => go(ats(), C3Cons(at(),acc))
    }

    Chapter3L.reverse(go(this,C3Nil))
  }

  // 5.2
  def take(n: Int): (C5Stream[A], C3List[A]) = {
    def leftovers_rtaken: (C5Stream[A], C3List[A]) = {
      @tailrec
      def go(depleting: C5Stream[A], acc: C3List[A], count: Int): (C5Stream[A], C3List[A]) = depleting match {
        case C5Empty => (C5Empty, acc)
        case _ if count >= n => (depleting, acc)
        case C5Cons(at, ats) => go(ats(), C3Cons(at(), acc), count + 1)
      }

      n match {
        case _ if n <= 0 => (this, C3Nil)
        case _ => go(this, C3Nil, 0)
      }
    }
    val (leftovers:C5Stream[A],rtaken:C3List[A]) = leftovers_rtaken
    (leftovers,Chapter3L.reverse(rtaken))
  }

  def drop(n: Int): C5Stream[A] = {
    @tailrec
    def go(depleting: C5Stream[A], acc: C3List[A], count: Int): C5Stream[A] = depleting match {
      case C5Empty => C5Empty
      case _ if count >= n => depleting
      case C5Cons(at, ats) => go(ats(), C3Cons(at(), acc), count + 1)
    }
    n match {
      case _ if n <= 0 => this
      case _ => go(this, C3Nil, 0)
    }
  }


}
case object C5Empty extends C5Stream[Nothing]
case class C5Cons[+A](h: () => A, t: () => C5Stream[A]) extends C5Stream[A]

object C5Stream {
  def cons[A](hd: => A, tl: => C5Stream[A]): C5Stream[A] = {
    lazy val head: A = hd
    lazy val tail: C5Stream[A] = tl
    C5Cons(() => head, () => tail)
  }
  def empty[A]: C5Stream[A] = C5Empty
  def apply[A](as: A*): C5Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}