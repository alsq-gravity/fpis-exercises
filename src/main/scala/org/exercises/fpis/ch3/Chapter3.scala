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
  def dropWhile[A](f: A => Boolean, l: C3List[A]): C3List[A] = l match {
    case C3Nil => l
    case C3Cons(x,_) if !f(x) => l
    case C3Cons(x,xs) if f(x) => dropWhile(f, xs)
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
  // using C3Nil as the initial value and C3Cons as the operation ==>>
  // foldRight mirrors the construction of a list
  // foldLeft mirrors the reversal of a list

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
  def concatByAppend[A](l1: C3List[A], l2: C3List[A]): C3List[A] = /* O(n^2) */ {
    @tailrec
    def go(accreting: C3List[A], depleting: C3List[A]) : C3List[A] = depleting match {
      case C3Nil => accreting
      case C3Cons(x,xs) => go(append(accreting,x),xs)
    }
    go(l1,l2)
  }
  def concat[A](l1: C3List[A], l2: C3List[A]): C3List[A] =  /* O(n) */ l2 match {
    case C3Nil => l1
    case C3Cons(x2,x2s) => reverse(l1) match {
      case C3Nil => l2
      case C3Cons(x1,x1s) => foldLeft[A,C3List[A]](C3Cons(x1,x1s),C3Cons(x2,x2s))((b:C3List[A],a:A) => C3Cons(a,b))
    }
  }
  def appendByConcat[A](l: C3List[A], item: A): C3List[A] = concat(l,C3Cons(item,C3Nil))

  // 3.15
  def flattenRecursing[A](ll: C3List[C3List[A]]): C3List[A] = {
    @tailrec
    def go(unwinding: C3List[C3List[A]], acc: C3List[A]): C3List[A] = unwinding match {
      case C3Nil => C3Nil
      case C3Cons(l,C3Nil) => concat(acc,l)
      case C3Cons(l,ls) => go(ls, concat(acc,l))
    }
    go(ll, C3Nil)
  }
  // vvv test for foldRightL
  def flatten[A](ll: C3List[C3List[A]]): C3List[A] =
    foldRightL[C3List[A],C3List[A]](ll,C3Nil:C3List[A])(concat(_,_))

  // 3.16
  def add1(in: C3List[Int]): C3List[Int] = foldRightL[Int,C3List[Int]](in,C3Nil:C3List[Int])((intIn,lout) => C3Cons(intIn+1,lout))

  // 3.17
  def d2s(in: C3List[Double]): C3List[String] = foldRightL[Double,C3List[String]](in,C3Nil:C3List[String])((dIn,lout) => C3Cons(dIn.toString,lout))

  // 3.18
  def c3map[A,B](in: C3List[A])(f: A => B): C3List[B] = foldRightL[A,C3List[B]](in,C3Nil:C3List[B])((a,lout) => C3Cons(f(a),lout))

  // 3.19
  def c3filter[A](in: C3List[A])(f: A => Boolean): C3List[A] = foldRightL[A,C3List[A]](in,C3Nil:C3List[A])((a,lout) => a match {
    case _ if f(a) =>  C3Cons(a,lout)
    case _ if !f(a) => lout
  })

  // 3.20
  def c3flatMap[A,B](in: C3List[A])(f: A => C3List[B]): C3List[B] = flatten(c3map[A,C3List[B]](in)(f))

  // 3.21
  def c3filterFM[A](in: C3List[A])(f: A => Boolean): C3List[A] = c3flatMap(in)(a => a match {
    case _ if f(a) =>  C3Cons(a,C3Nil)
    case _ if !f(a) => C3Nil
  })

  // 3.22 (simple, but not stack safe)
  def zipAddInt(a: C3List[Int], b: C3List[Int]): C3List[Int] = (a,b) match {
    case (C3Nil,_) => C3Nil
    case (_,C3Nil) => C3Nil
    case (C3Cons(xa,xas),C3Cons(xb,xbs)) => C3Cons(xa+xb, zipAddInt(xas,xbs))
  }

  // 3.23
  def take[A](la:C3List[A],n: Int): C3List[A] = {
    @tailrec
    def go(la:C3List[A], ix: Int, acc:C3List[A]): C3List[A] = la match {
      case C3Nil => acc
      case _ if ix >= n => acc
      case C3Cons(x,xs) => go(xs, ix+1, C3Cons(x,acc))
    }
    reverse(go(la,0,C3Nil:C3List[A]))
  }
  @tailrec
  def foldLeft2[A,B,C](la:C3List[A],lb:C3List[B],v:C)(op: (C, A, B) => C): C = (la, lb) match {
    case (C3Nil,_) => v
    case (_,C3Nil) => v
    case (C3Cons(xa,xas),C3Cons(xb,xbs)) => foldLeft2(xas,xbs, op(v, xa, xb))(op)
  }
  def foldRight2L[A,B,C](la:C3List[A],lb:C3List[B],v:C)(op: (A, B, C) => C): C = {
    val po: (C,A,B) => C = (c:C,a:A,b:B) => op(a,b,c)
    val minLen = Math.min(lenl(la),lenl(lb))
    foldLeft2(reverse(take(la,minLen)),reverse(take(lb,minLen)),v)(po)
  }
  // stack safe; also, testing this will test foldLeft2 and foldRight2L
  def c3zipWith[A,B,C](ina: C3List[A], inb: C3List[B])(f: (A,B) => C): C3List[C] = foldRight2L(ina,inb,C3Nil:C3List[C])((a:A, b:B, c:C3List[C]) => C3Cons(f(a,b),c))

  // not requested
  def takeWhile[A](la:C3List[A],f: A => Boolean): C3List[A] = {
    @tailrec
    def go(depleting:C3List[A], acc:C3List[A]): C3List[A] = depleting match {
      case C3Nil => acc
      case C3Cons(x,_) if !f(x) => acc
      case C3Cons(x,xs) => go(xs, C3Cons(x,acc))
    }
    reverse(go(la,C3Nil:C3List[A]))
  }

  // not requested
  def takeMatching[A](sample:A, presentation:C3List[A]): C3List[A] = {
    def doesMatch(i:A)(j:A): Boolean = i == j
    takeWhile(presentation,doesMatch(sample))
  }

  // 3.24 O(n*m); could be better
  @tailrec // <== the n part of O(n*m)
  def hasSubsequence[A](sup:C3List[A], sub:C3List[A]): Boolean = {

    @tailrec // <== the m part of O(n*m)
    def immediateSubsequence(isup:C3List[A], isub:C3List[A]): Boolean = isub match {
      // isup and isub must match in content and position
      case C3Nil => true // match and isub is shorter than isup
      case C3Cons(s,ss) => isup match {
        case C3Nil => false // match but isup is shorter than isub
        case C3Cons(x,xs) => s == x /* if this is a match */ &&
          /* try next */ immediateSubsequence(xs,ss) // or return false
      }
    }

    val (stub,predicate) = sub match {
      case C3Nil => (sup,true) // sub is empty: always a match
      case C3Cons(s,ss) =>
        def noMatch(sample:A)(item:A): Boolean = !(sample == item)
        val start = dropWhile(noMatch(s), sup) // drop all non-match prefix items
        start match {
          case C3Nil => (start, false) // sub begins with something not present in sup
          case _ => (start, immediateSubsequence(tail(start),ss))
        }
    }

    predicate match {
      case true => true
      case _ => hasSubsequence(tail(stub), sub) /* shift one and try again */
    }
  }

}
