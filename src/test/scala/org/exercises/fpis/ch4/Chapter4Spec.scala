package org.exercises.fpis.ch4

import org.specs2.mutable

object Chapter4Spec extends mutable.Specification {

  "map" should {

    def f(a: Int) = a*a

    "behave for Some" in {
      C4Some(3).map(f) must_=== C4Some(f(3))
    }
    "behave for None" in {
      C4None.map(f) must_=== C4None
    }
  }

  "flatMap" should {

    def fs(a: Int): C4Option[Int] = C4Some(a*a)
    def fn(a: Int): C4Option[Int] = {val _ = a; C4None }

    "behave for Some" in {
      C4Some(3).flatMap(fs) must_=== fs(3)
      C4Some(3).flatMap(fn) must_=== C4None
    }
    "behave for None" in {
      C4None.flatMap(fs) must_=== C4None
      C4None.flatMap(fn) must_=== C4None
    }
  }

  "getOrElse" should {

    val sDefault: C4Option[Int] = C4Some(5)
    val nDefault: C4Option[Int] = C4None

    "behave for Some" in {
      C4Some(3).getOrElse(sDefault) must_=== 3
      C4Some(3).getOrElse(nDefault) must_=== 3
    }
    "behave for None" in {
      C4None.getOrElse(sDefault) must_=== sDefault
      C4None.getOrElse(nDefault) must_=== nDefault
      nDefault.getOrElse(sDefault) must_=== sDefault
      nDefault.getOrElse(5) must_=== 5
      nDefault.getOrElse(nDefault) must_=== nDefault
    }
  }

  "orElse" should {

    val sDefault: C4Option[Int] = C4Some(5)
    val nDefault: C4Option[Int] = C4None

    "behave for Some" in {
      C4Some(3).orElse(sDefault) must_=== C4Some(3)
      C4Some(3).orElse(nDefault) must_=== C4Some(3)
    }
    "behave for None" in {
      C4None.orElse(sDefault) must_=== sDefault
      C4None.orElse(nDefault) must_=== nDefault
    }
  }

  "filter" should {

    val sDefault: C4Option[Int] = C4Some(5)
    val nDefault: C4Option[Int] = C4None

    "behave for Some" in {
      sDefault.filter(_ == 5) must_=== sDefault
      sDefault.filter(_ == 4) must_=== nDefault
    }
    "behave for None" in {
      nDefault.filter((_:Int) => true) must_=== nDefault
    }
  }

}
