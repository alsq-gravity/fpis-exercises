package org.exercises.fpis.ch2

import org.specs2._

object Chapter2Spec extends mutable.Specification {

  import Chapter2._

  "fibonacci" should {
    "return correctly first 6 fibonacci numbers" in {
      Range(0, 6, 1).map(fib(_)).toList must_=== List(BigInt(0), BigInt(1), BigInt(1), BigInt(2), BigInt(3), BigInt(5))
    }

    "return correctly first 9 negafibonacci numbers" in {
      Range(0, -9, -1).map(fib(_)).toList must_=== List(BigInt(0), BigInt(1), BigInt(-1), BigInt(2), BigInt(-3), BigInt(5), BigInt(-8), BigInt(13), BigInt(-21))
    }

    "return correctly fib(450)" in {
      // http://mathforum.org/library/drmath/view/52700.html
      fib(450) must_=== BigInt("4953967011875066473162524925231604047727791871346061001150551747313593851366517214899257280600")
    }

    "not blow up" in {
      fib(2000) > BigInt("4953967011875066473162524925231604047727791871346061001150551747313593851366517214899257280600") must beTrue
    }
  }

  "isSorted" should {
    "return true for empty array or array with one element" in {
      isSorted(Array.empty[Int],(a:Int, b:Int) => a < b) must beTrue
      isSorted(Array(1),(a:Int, b:Int) => a < b) must beTrue
    }

    "return true for sorted array of multiple elements" in {
      isSorted(Array(1,3),(a:Int, b:Int) => a < b) must beTrue
      isSorted(Array(1,3,5,6),(a:Int, b:Int) => a < b) must beTrue
    }

    "return false for unsorted array of multiple elements" in {
      isSorted(Array(3,1),(a:Int, b:Int) => a < b) must beFalse
      isSorted(Array(1,3,6,5,7,8,9),(a:Int, b:Int) => a < b) must beFalse
    }

  }
}