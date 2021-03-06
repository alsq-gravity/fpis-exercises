package org.exercises.fpis.ch4

import scala.annotation.tailrec


sealed trait C4Option[+A] {

  // 4.1
  def map[B](f: A => B): C4Option[B] = this match {
    case C4Some(a) => C4Some(f(a))
    case C4None => C4None
  }
  def flatMap[B](f: A => C4Option[B]): C4Option[B] = map(f).getOrElse(C4None)
  // vvv variation on flatten
  def getOrElse[B >: A](default: => B): B = this match {
    case C4Some(a) => a
    case C4None => default
  }
  def orElse[B >: A](ob: => C4Option[B]): C4Option[B] = map(C4Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): C4Option[A] = map(f).flatMap(pred => if(pred) this else C4None)
  def fold[B](ifEmpty: => B)(f: A => B): B = map(f).getOrElse(ifEmpty)
}
case class C4Some[+A](get: A) extends C4Option[A]
case object C4None extends C4Option[Nothing]

object C4Option {

  import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil}
  import org.exercises.fpis.ch3.Chapter3L._

  // 4.3
  def map2ap[A,B,C](ma: C4Option[A], mb: C4Option[B])(f: (A, B) => C): C4Option[C] = product(ma, mb).map(f.tupled)
  def map2[A,B,C](oa: C4Option[A], ob: C4Option[B])(f: (A,B) => C): C4Option[C] = oa.flatMap( a => ob.map( b => f(a,b)))

  def ap[A,B](ma: C4Option[A])(f: C4Option[A => B]): C4Option[B] = ma.flatMap(a => f.map(iF => iF(a)))
  def product[A,B](ma: C4Option[A], mb: C4Option[B]): C4Option[(A, B)] = {
    val f: C4Option[B => (A, B)] = ma.map(a => (b: B) => (a, b))
    ap(mb)(f)
  }

  def lift[A,B](f: A => B): C4Option[A] => C4Option[B] = _ map(f)
  def otry[A](a: => A): C4Option[A] = try C4Some(a) catch { case _:Exception => C4None }

  // 4.4
  def sequence[A](la:C3List[C4Option[A]]): C4Option[C3List[A]] = la match {
    case C3Nil => C4None
    case C3Cons(oa,C3Nil) => oa.map[C3List[A]](a => C3Cons(a,C3Nil))
    case C3Cons(oa,oas) => map2[A,C3List[A],C3List[A]](oa,sequence(oas))((a,as) => C3Cons(a,as))
  }
  def sequenceT[A,B](la:C3List[C4Option[A]]): C4Option[C3List[A]] = traverse[C4Option[A],A](la)(identity)

  // 4.5
  def traverseF[A, B](xa: C3List[A])(f: A => C4Option[B]): C4Option[C3List[B]] =
    foldRight[A,C4Option[C3List[B]]](xa,C4Some(C3Nil):C4Option[C3List[B]]){case (next, acc) => acc.flatMap(l => f(next).map(b => C3Cons(b,l)))} match {
      case C4Some(C3Nil) => C4None
      case res@_ => res
    }
  def traverse[A,B](la:C3List[A])(f: A => C4Option[B]): C4Option[C3List[B]] = {
    @tailrec // recursion necessary for breaking on first occurrence of C4None (fold may not do that)
    def go(depleting:C3List[A], acc:C4Option[C3List[B]]): C4Option[C3List[B]] = depleting match {
      case C3Nil => acc.filter(bs => bs match {
        case C3Nil => false
        case _ => true
      })
      case C3Cons(a,as) =>
        // prettier, but not tail recursive:
        // f(a).fold[C4Option[C3List[B]]](C4None)(b => go(as,C4Some(C3Cons(b, acc.getOrElse(C3Nil)))))
        f(a) match {
          case C4None => C4None
          case C4Some(b) => go(as,C4Some(C3Cons(b, acc.getOrElse(C3Nil))))
        }
    }
    go(la,C4Some(C3Nil)).map(bs => reverse(bs))
  }
}
