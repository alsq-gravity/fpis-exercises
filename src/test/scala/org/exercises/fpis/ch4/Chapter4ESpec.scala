package org.exercises.fpis.ch4

import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil}
import org.specs2.mutable

object Chapter4ESpec extends mutable.Specification {

  "map2" should {

    import C4Either._

    def s2i(a: String): Int = a.toInt

    val nsDefault: C4Left[Exception] = C4Left(new NumberFormatException())
    def i2(a: Int, b: String): Double  = (a+s2i(b)).toDouble

    "behave for Right,Right" in {
      map2(etry(s2i("1")),C4Right("2"))(i2) must_=== C4Right(3.0)
    }
    "behave for Right,Left" in {
      map2(etry(s2i("1")),nsDefault)(i2) must_=== nsDefault
    }
    "behave for Left,Right" in {
      map2(etry(s2i("1.5")),C4Right("2"))(i2) match {
        case C4Left(_) => ok
        case _ => ko
      }
    }
    "behave for Left,Left" in {
      map2(etry(s2i("1.5")),nsDefault)(i2) match {
        case C4Left(_) => ok
        case _ => ko
      }
    }
  }

  "traverse" should {

    import C4Either.{traverse, etry}

    val ls1: C3List[String] = C3Cons("1", C3Nil)
    val ls2: C3List[String] = C3Cons("1", C3Cons("2", C3Nil))
    val ls3: C3List[String] = C3Cons("1", C3Cons("2", C3Cons("3", C3Nil)))
    val lsp3: C3List[String] = C3Cons("1", C3Cons("2.5", C3Cons("3", C3Nil)))
    val li1: C3List[Int] = C3Cons(1, C3Nil)
    val li2: C3List[Int] = C3Cons(1, C3Cons(2, C3Nil))
    val li3: C3List[Int] = C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))
    def s2ei(s: String): C4Either[Exception,Int] = etry(s.toInt)

    "behave for list of sizes 1,2,3" in {
      traverse(ls1)(s2ei) must_=== C4Right(li1)
      traverse(ls2)(s2ei) must_=== C4Right(li2)
      traverse(ls3)(s2ei) must_=== C4Right(li3)
    }
    "behave for empty list" in {
      traverse(C3Nil)(s2ei) must_=== C4Right(C3Nil)
    }
    "behave for list with poison pill" in {
      traverse(lsp3)(s2ei)  match {
        case C4Left(_) => ok
        case _ => ko
      }
    }
  }

  "traverseR" should {

    import C4Either.{traverseR, etry}

    val ls1: C3List[String] = C3Cons("1", C3Nil)
    val ls2: C3List[String] = C3Cons("1", C3Cons("2", C3Nil))
    val ls3: C3List[String] = C3Cons("1", C3Cons("2", C3Cons("3", C3Nil)))
    val lsp3: C3List[String] = C3Cons("1", C3Cons("2.5", C3Cons("3", C3Nil)))
    val li1: C3List[Int] = C3Cons(1, C3Nil)
    val li2: C3List[Int] = C3Cons(1, C3Cons(2, C3Nil))
    val li3: C3List[Int] = C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))
    def s2ei(s: String): C4Either[Exception,Int] = etry(s.toInt)

    "behave for list of sizes 1,2,3" in {
      traverseR(ls1)(s2ei) must_=== C4Right(li1)
      traverseR(ls2)(s2ei) must_=== C4Right(li2)
      traverseR(ls3)(s2ei) must_=== C4Right(li3)
    }
    "behave for empty list" in {
      traverseR(C3Nil)(s2ei) must_=== C4Right(C3Nil)
    }
    "behave for list with poison pill" in {
      traverseR(lsp3)(s2ei)  match {
        case C4Left(_) => ok
        case _ => ko
      }
    }
  }

  "sequence" should {

    import C4Either.sequence

    val l1e: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Nil)
    val el1: C4Either[Exception,C3List[Int]] = C4Right(C3Cons(1, C3Nil))
    val l2e: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Cons(C4Right(2), C3Nil))
    val el2: C4Either[Exception,C3List[Int]] = C4Right(C3Cons(1, C3Cons(2, C3Nil)))
    val l3e: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Cons(C4Right(2), C3Cons(C4Right(3),C3Nil)))
    val l3ep: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Cons(C4Right(2), C3Cons(C4Left(new RuntimeException()), C3Nil)))
    val el3: C4Either[Exception,C3List[Int]] = C4Right(C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))

    "behave for list of sizes 1,2,3" in {
      sequence(l1e) must_=== el1
      sequence(l2e) must_=== el2
      sequence(l3e) must_=== el3
    }
    "behave for empty list" in {
      sequence(C3Nil) must_=== C4Right(C3Nil)
    }
    "behave for list with poison pill" in {
      sequence(l3ep) match {
        case C4Left(_) => ok
        case _ => ko
      }
    }
  }

  "sequenceR" should {

    import C4Either.sequenceR

    val l1e: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Nil)
    val el1: C4Either[Exception,C3List[Int]] = C4Right(C3Cons(1, C3Nil))
    val l2e: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Cons(C4Right(2), C3Nil))
    val el2: C4Either[Exception,C3List[Int]] = C4Right(C3Cons(1, C3Cons(2, C3Nil)))
    val l3e: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Cons(C4Right(2), C3Cons(C4Right(3),C3Nil)))
    val l3ep: C3List[C4Either[Exception,Int]] = C3Cons(C4Right(1), C3Cons(C4Right(2), C3Cons(C4Left(new RuntimeException()), C3Nil)))
    val el3: C4Either[Exception,C3List[Int]] = C4Right(C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))

    "behave for list of sizes 1,2,3" in {
      sequenceR(l1e) must_=== el1
      sequenceR(l2e) must_=== el2
      sequenceR(l3e) must_=== el3
    }
    "behave for empty list" in {
      sequenceR(C3Nil) must_=== C4Right(C3Nil)
    }
    "behave for list with poison pill" in {
      sequenceR(l3ep) match {
        case C4Left(_) => ok
        case _ => ko
      }
    }
  }

}
