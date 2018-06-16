package org.exercises.fpis.ch5

import org.exercises.fpis.ch3.{C3Cons, C3Nil}
import org.specs2.mutable

object Chapter5Spec  extends mutable.Specification {

  import C5Stream._

  val si1 = cons(3, C5Empty)
  val si2 = cons(2, si1)
  val si3 = cons(1, si2)
  val li1 = C3Cons(3, C3Nil)
  val li2 = C3Cons(2, li1)
  val li3 = C3Cons(1, li2)

  "toList" should {

    "behave for 1,2,3 elements" in {
      si1.toList must_=== li1
      si2.toList must_=== li2
      si3.toList must_=== li3
    }
    "behave for empty" in {
      C5Empty.toList must_=== C3Nil
    }
  }

  "take" should {

    "behave for 1,2,3 elements" in {
      si1.take(1) must_=== ((C5Empty, li1))
      si2.take(2) must_=== ((C5Empty, li2))
      si3.take(3) must_=== ((C5Empty, li3))
    }
    "behave for empty" in {
      C5Empty.take(0) must_=== ((C5Empty, C3Nil))
    }
    "behave for large n" in {
      si2.take(20) must_=== ((C5Empty, li2))
    }

    "behave for partial n" in {
      val (left21, taken21) = si2.take(1)
      left21.toList must_=== C3Cons(3, C3Nil)
      taken21 must_=== C3Cons(2, C3Nil)

      val (left31, taken31) = si3.take(1)
      left31.toList must_=== C3Cons(2, C3Cons(3, C3Nil))
      taken31 must_=== C3Cons(1, C3Nil)
    }
  }

  "drop" should {

    "behave for 1,2,3 elements" in {
      si1.drop(1) must_=== C5Empty
      si2.drop(2) must_=== C5Empty
      si3.drop(3) must_=== C5Empty
    }
    "behave for empty" in {
      C5Empty.drop(0) must_=== C5Empty
    }
    "behave for large n" in {
      si2.drop(20) must_=== C5Empty
    }

    "behave for partial n" in {
      si2.drop(1).toList must_=== C3Cons(3, C3Nil)
      si3.drop(1).toList must_=== C3Cons(2, C3Cons(3, C3Nil))
    }
  }
}