package org.exercises.fpis.ch3

import scala.annotation.tailrec


sealed trait C3List[+A]
case object C3Nil extends C3List[Nothing]
case class C3Cons[+A](head: A, tail: C3List[A]) extends C3List[A]

object Chapter3 {

  // 3.1 ignored

  // 3.2
  def tail[A](l: C3List[A]): C3List[A] = l match {
    case C3Nil => l
    case C3Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](head: A, l: C3List[A]): C3List[A] =  l match {
    case C3Nil => l
    case C3Cons(_, xs) => C3Cons(head,xs)
  }

  // 3.4
  @tailrec
  def drop[A](n: Int, l: C3List[A]): C3List[A] = (n,l) match {
    case (bad, ll) if bad < 0 => ll
    case (_, C3Nil) => C3Nil
    case (0,ll) => ll
    case (tbd,C3Cons(_,xs)) => drop(tbd-1,xs)
  }

  // 3.5
  @tailrec
  def dropWhile[A](l: C3List[A], f: A => Boolean): C3List[A] = l match {
    case C3Nil => l
    case C3Cons(x,_) if !f(x) => l
    case C3Cons(x,xs) if f(x) => dropWhile(xs,f)
  }

  // not assigned
  def reverse[A](l: C3List[A]): C3List[A] = {

    @tailrec
    def go(l: C3List[A], acc: C3List[A]): C3List[A] = l match {
      case C3Nil => acc
      case C3Cons(x,xs) => go(xs, C3Cons(x, acc))
    }

    go(l,C3Nil)

  }

  // 3.6
  def init[A](l: C3List[A]): C3List[A] = {

    @tailrec
    def go(depleting: C3List[A], acc: C3List[A]): C3List[A] = depleting match {
      case C3Nil => acc
      case C3Cons(prev,xs) => xs match {
        case C3Nil => acc
        case C3Cons(_,C3Nil) => C3Cons(prev,acc)
        case C3Cons(x,xs) => go(xs,C3Cons(x,C3Cons(prev,acc)))
      }
    }

    l match {
      case C3Nil => l
      case C3Cons(_ /*last */,C3Nil) => C3Nil
      case _ => reverse (go(l,C3Nil))
    }
  }
  def init1[A](l: C3List[A]): C3List[A] = reverse(tail(reverse(l)))


  // 3.7: No

  // not assigned
  def foldRight[A,B](l:C3List[A],v:B)(op: (A,B) => B): B = l match {
    case C3Nil => v
    case C3Cons(x,xs) => op(x, foldRight(xs,v)(op))
  }

  // not assigned
  @tailrec
  def foldLeft[A,B](l:C3List[A],v:B)(op: (B,A) => B): B = l match {
    case C3Nil => v
    case C3Cons(x,xs) => foldLeft(xs,op(v,x))(op)
  }

  // 3.8 see specs

  // 3.9
  def lenr[A](l:C3List[A]): Int = foldRight(l,0)((_:A, init:Int) => init+1)

  // 3.10 I am convinced

  // 3.11
  def lenl[A](l:C3List[A]): Int = foldLeft(l,0)((init:Int, _:A) => init+1)
  def suml(l:C3List[Int]): Int = foldLeft(l,0)((last:Int, v: Int) => last+v)
  def prodl(l:C3List[Int]): Int = foldLeft(l,1)((last:Int, v: Int) => last*v)

  // 3.12
  def frevl[A](l:C3List[A]): C3List[A] = foldLeft(l,C3Nil:C3List[A])((last:C3List[A],curr:A) => C3Cons[A](curr,last))

  // 3.13
  def foldRightL[A,B](l:C3List[A],v:B)(op: (A,B) => B): B = {
    def po: (B,A) => B = (b:B, a:A) => op(a,b) // needs be associative and commutative
    foldLeft(reverse(l),v)(po)
  }
  def foldLeftR[A,B](l:C3List[A],v:B)(op: (B,A) => B): B = {
    def po: (A,B) => B = (a:A, b:B) => op(b,a) // needs be associative and commutative
    foldRight(reverse(l),v)(po)
  }

  // 3.14
  def append[A](l: C3List[A], item: A): C3List[A] = reverse(C3Cons(item, reverse(l)))
  def appendL[A](l: C3List[A], item: A): C3List[A] = foldLeft[A,C3List[A]](reverse(l),C3Cons(item,C3Nil):C3List[A])((b,a) => C3Cons(a,b))
  // vvv test for foldLeftR
  def appendLL[A](l: C3List[A], item: A): C3List[A] = foldLeftR[A,C3List[A]](reverse(l),C3Cons(item,C3Nil):C3List[A])((b,a) => C3Cons(a,b))
  def concat[A](l1: C3List[A], l2: C3List[A]): C3List[A] = {
    @tailrec
    def go(accreting: C3List[A], depleting: C3List[A]) : C3List[A] = depleting match {
      case C3Nil => accreting
      case C3Cons(x,xs) => go(append(accreting,x),xs)
    }
    go(l1,l2)
  }

  // 3.15
  def flatten[A](ll: C3List[C3List[A]]): C3List[A] = {
    @tailrec
    def go(plain: C3List[A], unwinding: C3List[C3List[A]]): C3List[A] = unwinding match {
      case C3Nil => C3Nil
      case C3Cons(l,C3Nil) => concat(plain,l)
      case C3Cons(l,ls) => go(concat(plain,l),ls)
    }
    go(C3Nil,ll)
  }
}
