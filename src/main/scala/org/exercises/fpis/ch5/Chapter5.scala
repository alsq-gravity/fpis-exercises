package org.exercises.fpis.ch5

import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil, Chapter3L}
import org.exercises.fpis.ch4.{C4None, C4Option, C4Some}

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
  def takeL(n: Int): (C5Stream[A], C3List[A]) = {
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

  def take(n: Int): C5Stream[A] = this match {
    case C5Empty => C5Empty
    case _ if n == 0 => C5Empty
    case C5Cons(h, t) => C5Stream.cons(h(), t().take(n-1))
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

  // 5.3
  def takeWhile(p: A => Boolean): C5Stream[A] = this match {
    case C5Cons(at, ats) if p(at()) => C5Stream.cons[A](at(), ats().takeWhile(p))
    case _ => C5Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case C5Cons(at, ats) => f(at(), ats().foldRight(z)(f))
    case _ => z
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = {
    @tailrec
    def go(depleting: C5Stream[A]): Boolean = depleting match {
      case C5Empty => true
      case C5Cons(at, _) if !p(at()) => false
      case C5Cons(_, ats) => go(ats())
    }
    go(this)
  }

  def exists(p: A => Boolean): Boolean = {
    @tailrec
    def go(depleting: C5Stream[A]): Boolean = depleting match {
      case C5Empty => false
      case C5Cons(at, _) if p(at()) => true
      case C5Cons(_, ats) => go(ats())
    }
    go(this)
  }

  // 5.5
  def takeWhileR(p: A => Boolean): C5Stream[A] = {
    def f(a: A, s: => C5Stream[A]): C5Stream[A] = if (p(a)) C5Stream.cons(a,s) else C5Stream.empty
    foldRight(C5Stream.empty[A])(f)
  }

  // 5.6
  def headOption: C4Option[A] = {
    def frf(a: A, s: => C4Option[A]): C4Option[A] = this match {
      case C5Empty => s
      case _ => C4Some(a)
    }
    val frf1 : (A, => C4Option[A]) => C4Option[A] = frf
    foldRight[C4Option[A]](C4None:C4Option[A])(frf1)
  }

  // 5.7
  def map[B](f: A => B): C5Stream[B] = {
    def frf(a: A, s: => C5Stream[B]): C5Stream[B] = C5Stream.cons[B](f(a),s)
    foldRight[C5Stream[B]](C5Stream.empty[B])(frf)
  }

  def filter(p: A => Boolean): C5Stream[A] = {
    def frf(a: A, s: => C5Stream[A]): C5Stream[A] = if(p(a)) C5Stream.cons(a,s) else s
    foldRight[C5Stream[A]](C5Stream.empty[A])(frf)
  }

  def append[B>:A](as: C5Stream[B]): C5Stream[B] = {
    def frf(a: A, s: => C5Stream[B]): C5Stream[B] = C5Stream.cons[B](a,s)
    foldRight[C5Stream[B]](as)(frf)
  }

  def flatMap[B](f: A => C5Stream[B]): C5Stream[B] = {
    def frf(a: A, s: => C5Stream[B]): C5Stream[B] = f(a).append[B](s)
    foldRight[C5Stream[B]](C5Stream.empty[B])(frf)
  }


  // 5.13
  def unfoldMap[B](f: A => B): C5Stream[B] = {
    def next(aas: C5Stream[A]): C4Option[(B, C5Stream[A])] = aas.headOption.map(a => (f(a),aas.drop(1)))
    C5Stream.unfold[B,C5Stream[A]](this)(next)
  }
  def unfoldTake(n: Int): C5Stream[A] = {
    def next(aas: C5Stream[A], count: Int): C4Option[(A, (C5Stream[A],Int))] = aas.headOption.flatMap(a => if (count<n) C4Some((a,(aas.drop(1),count+1))) else C4None)
    C5Stream.unfold[A,(C5Stream[A],Int)]((this,0))(z => next(z._1,z._2))
  }
  def unfoldTakeWhile(p: A => Boolean): C5Stream[A] = {
    def next(aas: C5Stream[A]): C4Option[(A, C5Stream[A])] = aas.headOption.flatMap(a => if (p(a)) C4Some((a,aas.drop(1))) else C4None)
    C5Stream.unfold[A,C5Stream[A]](this)(next)
  }

  // 5.14
  @tailrec
  final def startsWith[B >: A](s: C5Stream[B]): Boolean = {
    C5Stream.c5zipWith(this,s)((a1:A, a2:B) => a1 == a2) match {
      case C5Empty => true
      case C5Cons(a,_) => if (!a()) false else this.drop(1).startsWith(s.drop(1))
    }
  }

  // 5.15
  def tails: C5Stream[C5Stream[A]] = {
    def next(aas: C5Stream[A]): C4Option[(C5Stream[A], C5Stream[A])] = aas.headOption.map(_ => (aas,aas.drop(1)))
    C5Stream.unfold[C5Stream[A],C5Stream[A]](this)(next)
  }

  // 5.16
  def hasSubsequence[B >: A](s: C5Stream[B]): Boolean = tails.exists(_.startsWith(s))

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

  // 5.8
  def constant[A](a: A): C5Stream[A] = C5Stream.cons(a, constant(a))
  // 5.9
  def from(a: Int): C5Stream[Int] = C5Stream.cons[Int](a, C5Stream.from(a+1))
  // 5.10
  // (0) 1 1 2 3 5 ... f(n+1) = f(n) + f(n-1)
  val fibSeqFrom1: C5Stream[Int] = {
    def fibNext(fibN: Int, fibNmin1: Int): C5Stream[Int] = C5Stream.cons[Int](fibN, fibNext(fibN + fibNmin1, fibN))
    fibNext(1,0)
  }

  // 5.11
  def unfold[A,S](z:S)(f: S => C4Option[(A,S)]): C5Stream[A] = {
    def next(aa:A, ss:S): C5Stream[A] = C5Stream.cons(aa,unfold(ss)(f))
    f(z).map[C5Stream[A]]{case(a,s) => next(a,s)}.getOrElse(C5Stream.empty[A])
  }

  // 5.12
  def unfoldOnes: C5Stream[Int] = unfold[Int,Unit](())((_: Unit) => C4Some((1,())))
  def unfoldConst[A](const:A): C5Stream[A] = unfold[A,Unit](())((_: Unit) => C4Some((const,())))
  def unfoldFrom(start:Int): C5Stream[Int] = unfold[Int,Int](start)((current: Int) => {
    val nextStart = current+1
    C4Some((current,nextStart))
  })
  def unfoldFibFrom2: C5Stream[Int] = {
    def next(nMin1: Int, n: Int): C4Option[(Int, (Int, Int))] = C4Some((nMin1+n, (n, nMin1+n)))
    unfold[Int, (Int, Int)]((0, 1))((curr: (Int, Int)) => next(curr._1, curr._2))
  }

  // 5.13
  def c5zipWith[A,B,C](ina: C5Stream[A], inb: C5Stream[B])(f: (A,B) => C): C5Stream[C] = {
    val maybeC: C4Option[C] = C4Option.map2(ina.headOption,inb.headOption)(f)
    maybeC.fold(C5Stream.empty[C])((c:C) => C5Stream.cons[C](c,c5zipWith(ina.drop(1),inb.drop(1))(f)))
  }
  def unfoldZipAll[A,B](ina: C5Stream[A], inb: C5Stream[B]): C5Stream[(C4Option[A],C4Option[B])] = {
    type value = (C4Option[A],C4Option[B])
    type state = (C5Stream[A], C5Stream[B])
    def next(aas:C5Stream[A], bbs:C5Stream[B]): C4Option[(value, state)] = (aas.headOption, bbs.headOption) match {
      case (C4None,C4None) => C4None
      case _ => C4Some(((aas.headOption,bbs.headOption),(aas.drop(1),bbs.drop(1))))
    }
    unfold[value, state]((ina, inb))(as_bs => next(as_bs._1, as_bs._2))
  }

}