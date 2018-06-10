package org.exercises.fpis.ch4

import org.exercises.fpis.ch3.{C3Cons, C3Nil}
import org.specs2.mutable

object Chapter4VSpec extends mutable.Specification {

  import Chapter4V._

  "mean" should {

    "behave for Some" in {
      mean(C3Cons(1.0, C3Cons(2.0, C3Cons(4.0, C3Cons(8.0, C3Nil))))) must_=== C4Some(3.75)
      mean(C3Cons(1.0, C3Cons(2.0, C3Cons(3.0, C3Cons(4.0, C3Cons(5.0, C3Cons(6.0,C3Nil))))))) must_=== C4Some(7.0/2.0)
      mean(C3Cons(2.0, C3Nil)) must_=== C4Some(2.0)
    }
    "behave for None" in {
      mean(C3Nil) must_=== C4None
    }
  }

  "variance" should {

    "behave for Some" in {
      val diceVar = variance(C3Cons(1.0, C3Cons(2.0, C3Cons(3.0, C3Cons(4.0, C3Cons(5.0, C3Cons(6.0,C3Nil)))))))
      diceVar.filter(vr => vr > 2.916 && vr < 2.92) must_!== C4None
      variance(C3Cons(2.0, C3Nil)) must_=== C4Some(0.0)
    }
    "behave for None" in {
      mean(C3Nil) must_=== C4None
    }
  }


}
