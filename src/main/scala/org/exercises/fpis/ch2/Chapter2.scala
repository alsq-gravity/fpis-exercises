package org.exercises.fpis.ch2

import scala.annotation.tailrec

object Chapter2 {
  // 2.1
  def fib(n: Int): BigInt = {
    @tailrec
    def go4fib(counter: Int, a: BigInt, b: BigInt, memo: Map[Int,BigInt]): BigInt = {
      val next = a+b
      if (counter < n) go4fib(counter+1, b, next, memo+((counter+1) -> next))
      else memo(counter)
    }
    @tailrec
    def go4negfib(counter: Int, a: BigInt, b: BigInt, memo: Map[Int,BigInt]): BigInt = {
      val next = a+b
      if (counter < -n) go4negfib(counter+1, b, next, memo+((counter+1) -> next))
      else if ( 0 == ((-n+1) % 2)) memo(counter) else -memo(counter)
    }
    n match {
      case 0 => BigInt(0)
      case 1 | -1 => BigInt(1)
      case _ if n > 0 => go4fib( 2,BigInt(1),BigInt(1),Map(2 -> BigInt(1)))
      // https://en.wikipedia.org/wiki/Fibonacci_number
      case _ if n < 0 => go4negfib( 2,BigInt(1),BigInt(1),Map(2 -> BigInt(1)))
    }
  }
  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def traverseWhileTrue(last: Boolean, len: Int)(ary: Array[A], ix: Int, op: (A,A) => Boolean): Boolean = len match {
      case _ if !last => false
      case _ if ix >= len => last
      case _ => traverseWhileTrue(op(ary(ix-1), ary(ix)), len)(ary,ix+1,op)
    }
    traverseWhileTrue(true,as.length)(as,1,ordered)
  }
  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)
  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A ,B) => C = (a:A, b:B) => f(a)(b)
  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))
}
