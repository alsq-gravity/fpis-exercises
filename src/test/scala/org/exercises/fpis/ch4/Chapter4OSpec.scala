package org.exercises.fpis.ch4

import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil}
import org.specs2.mutable

object Chapter4OSpec extends mutable.Specification {

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

  "fold" should {

    val sDefault: C4Option[Int] = C4Some(5)
    val nDefault: C4Option[Int] = C4None
    def f(a: Int): Int = a*a

    "behave for Some" in {
      sDefault.fold[Int](2)(f) must_=== f(5)
    }
    "behave for None" in {
      nDefault.fold[Int](2)(f) must_=== 2
    }
  }


  "lift" should {

    import C4Option.lift

    val sDefault: C4Option[Int] = C4Some(5)
    val niDefault: C4Option[Int] = C4None
    val nsDefault: C4Option[String] = C4None
    def i2s(a: Int): String = a.toString
    val oi2os: C4Option[Int] => C4Option[String] = lift(i2s)

    "behave for Some" in {
      oi2os(sDefault) must_=== C4Some("5")
    }
    "behave for None" in {
      oi2os(niDefault) must_=== nsDefault
    }
  }

  "otry" should {

    import C4Option.otry

    val niDefault: C4Option[Int] = C4None
    def s2i(a: String): Int = a.toInt

    "behave for Some" in {
      otry(s2i("5")) must_=== C4Some(5)
    }
    "behave for None" in {
      otry(s2i("5.0")) must_=== niDefault
    }
  }

  "map2" should {

    import C4Option._

    def s2i(a: String): Int = a.toInt

    val nsDefault: C4Option[String] = C4None
    def i2(a: Int, b: String): Double  = (a+s2i(b)).toDouble

    "behave for Some,Some" in {
      map2(otry(s2i("1")),C4Some("2"))(i2) must_=== C4Some(3.0)
    }
    "behave for Some,None" in {
      map2(otry(s2i("1")),nsDefault)(i2) must_=== C4None
    }
    "behave for None,Some" in {
      map2(otry(s2i("1.5")),C4Some("2"))(i2) must_=== C4None
    }
    "behave for None,None" in {
      map2(otry(s2i("1.5")),nsDefault)(i2) must_=== C4None
    }
  }

  "sequence" should {

    import C4Option.sequence

    val l1o: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Nil)
    val ol1: C4Option[C3List[Int]] = C4Some(C3Cons(1, C3Nil))
    val l2o: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Cons(C4Some(2), C3Nil))
    val ol2: C4Option[C3List[Int]] = C4Some(C3Cons(1, C3Cons(2, C3Nil)))
    val l3o: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Cons(C4Some(2), C3Cons(C4Some(3),C3Nil)))
    val l3op: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Cons(C4Some(2), C3Cons(C4None, C3Nil)))
    val ol3: C4Option[C3List[Int]] = C4Some(C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))

    "behave for list of sizes 1,2,3" in {
      sequence(l1o) must_=== ol1
      sequence(l2o) must_=== ol2
      sequence(l3o) must_=== ol3
    }
    "behave for empty list" in {
      sequence(C3Nil) must_=== C4None
    }
    "behave for list with poison pill" in {
      sequence(l3op) must_=== C4None
    }
  }

  "traverse" should {

    import C4Option.{traverse, otry}

    val ls1: C3List[String] = C3Cons("1", C3Nil)
    val ls2: C3List[String] = C3Cons("1", C3Cons("2", C3Nil))
    val ls3: C3List[String] = C3Cons("1", C3Cons("2", C3Cons("3", C3Nil)))
    val lsp3: C3List[String] = C3Cons("1", C3Cons("2.5", C3Cons("3", C3Nil)))
    val li1: C3List[Int] = C3Cons(1, C3Nil)
    val li2: C3List[Int] = C3Cons(1, C3Cons(2, C3Nil))
    val li3: C3List[Int] = C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))
    def s2oi(s: String): C4Option[Int] = otry(s.toInt)

    "behave for list of sizes 1,2,3" in {
      traverse(ls1)(s2oi) must_=== C4Some(li1)
      traverse(ls2)(s2oi) must_=== C4Some(li2)
      traverse(ls3)(s2oi) must_=== C4Some(li3)
    }
    "behave for empty list" in {
      traverse(C3Nil)(s2oi) must_=== C4None
    }
    "behave for list with poison pill" in {
      traverse(lsp3)(s2oi) must_=== C4None
    }
  }

  "traverseF" should {

    import C4Option.{traverseF, otry}

    val ls1: C3List[String] = C3Cons("1", C3Nil)
    val ls2: C3List[String] = C3Cons("1", C3Cons("2", C3Nil))
    val ls3: C3List[String] = C3Cons("1", C3Cons("2", C3Cons("3", C3Nil)))
    val lsp3: C3List[String] = C3Cons("1", C3Cons("2.5", C3Cons("3", C3Nil)))
    val li1: C3List[Int] = C3Cons(1, C3Nil)
    val li2: C3List[Int] = C3Cons(1, C3Cons(2, C3Nil))
    val li3: C3List[Int] = C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))
    def s2oi(s: String): C4Option[Int] = otry(s.toInt)

    "behave for list of sizes 1,2,3" in {
      traverseF(ls1)(s2oi) must_=== C4Some(li1)
      traverseF(ls2)(s2oi) must_=== C4Some(li2)
      traverseF(ls3)(s2oi) must_=== C4Some(li3)
    }
    "behave for empty list" in {
      traverseF(C3Nil)(s2oi) must_=== C4None
    }
    "behave for list with poison pill" in {
      traverseF(lsp3)(s2oi) must_=== C4None
    }
  }

  "sequenceT" should {

    import C4Option.sequenceT

    val l1o: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Nil)
    val ol1: C4Option[C3List[Int]] = C4Some(C3Cons(1, C3Nil))
    val l2o: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Cons(C4Some(2), C3Nil))
    val ol2: C4Option[C3List[Int]] = C4Some(C3Cons(1, C3Cons(2, C3Nil)))
    val l3o: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Cons(C4Some(2), C3Cons(C4Some(3),C3Nil)))
    val l3op: C3List[C4Option[Int]] = C3Cons(C4Some(1), C3Cons(C4Some(2), C3Cons(C4None, C3Nil)))
    val ol3: C4Option[C3List[Int]] = C4Some(C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))

    "behave for list of sizes 1,2,3" in {
      sequenceT(l1o) must_=== ol1
      sequenceT(l2o) must_=== ol2
      sequenceT(l3o) must_=== ol3
    }
    "behave for empty list" in {
      sequenceT(C3Nil) must_=== C4None
    }
    "behave for list with poison pill" in {
      sequenceT(l3op) must_=== C4None
    }
  }
}
