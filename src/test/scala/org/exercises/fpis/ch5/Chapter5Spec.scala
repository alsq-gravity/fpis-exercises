package org.exercises.fpis.ch5

import org.exercises.fpis.ch3.{C3Cons, C3List, C3Nil}
import org.exercises.fpis.ch4.{C4None, C4Some}
import org.specs2.mutable

object Chapter5Spec  extends mutable.Specification {

  import C5Stream._
  import org.exercises.fpis.ch3.Chapter3L

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

  "takeL" should {

    "behave for 1,2,3 elements" in {
      si1.takeL(1) must_=== ((C5Empty, li1))
      si2.takeL(2) must_=== ((C5Empty, li2))
      si3.takeL(3) must_=== ((C5Empty, li3))
    }
    "behave for empty" in {
      C5Empty.takeL(0) must_=== ((C5Empty, C3Nil))
    }
    "behave for large n" in {
      si2.takeL(20) must_=== ((C5Empty, li2))
    }

    "behave for partial n" in {
      val (left21, taken21) = si2.takeL(1)
      left21.toList must_=== C3Cons(3, C3Nil)
      taken21 must_=== C3Cons(2, C3Nil)

      val (left31, taken31) = si3.takeL(1)
      left31.toList must_=== C3Cons(2, C3Cons(3, C3Nil))
      taken31 must_=== C3Cons(1, C3Nil)
    }
  }

  "take" should {

    "behave for 1,2,3 elements" in {
      si1.take(1).toList must_=== li1
      si2.take(2).toList must_=== li2
      si3.take(3).toList must_=== li3
    }
    "behave for empty" in {
      C5Empty.take(0) must_=== C5Empty
    }
    "behave for large n" in {
      si2.take(20).toList must_=== li2
    }

    "behave for partial n" in {
      si2.take(1).toList must_=== C3Cons(2, C3Nil)
      si3.take(1).toList must_=== C3Cons(1, C3Nil)
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

  "takeWhile" should {

    "behave for 2 elements" in {
      val sample: C3List[Int] = si3.takeWhile(a => a <= 2).toList
      sample must_=== C3Cons(1, C3Cons(2, C3Nil))
    }
    "behave for empty" in {
      val e = C5Empty:C5Stream[Int]
      e.takeWhile(_ => true) must_=== C5Empty
    }
  }

  "forAll" should {

    "behave for 2 elements" in {
      si3.forAll(a => a <= 2) must beFalse
      si3.forAll(a => a <= 3) must beTrue
    }
    "behave for empty" in {
      val e = C5Empty:C5Stream[Int]
      e.forAll(_ => false) must beTrue
    }
  }

  "takeWhileR" should {

    "behave for 2 elements" in {
      val sample: C3List[Int] = si3.takeWhileR(a => a <= 2).toList
      sample must_=== C3Cons(1, C3Cons(2, C3Nil))
    }
    "behave for empty" in {
      val e = C5Empty:C5Stream[Int]
      e.takeWhileR(_ => true) must_=== C5Empty
    }
  }

  "headOption" should {

    "behave for 1,2,3 elements" in {
      si1.headOption must_=== C4Some(li1.head)
      si2.headOption must_=== C4Some(li2.head)
      si3.headOption must_=== C4Some(li3.head)
    }
    "behave for empty" in {
      C5Empty.headOption must_=== C4None
    }
  }

  "map" should {
    import Chapter3L.c3map
    def f(i: Int): String = i.toString

    "behave for 1,2,3 elements" in {
      si1.map(f).toList must_=== c3map(li1)(f)
      si2.map(f).toList must_=== c3map(li2)(f)
      si3.map(f).toList must_=== c3map(li3)(f)
    }
    "behave for empty" in {
      C5Empty.map(f) must_=== C5Empty
    }
  }

  "filter" should {
    import Chapter3L.c3filter
    def f(a: Int): Boolean = 0 == a%2

    "remove all odds" in {
      si1.filter(f).toList must_=== c3filter(li1)(f)
      si2.filter(f).toList must_=== c3filter(li2)(f)
      si3.filter(f).toList must_=== c3filter(li3)(f)
    }
    "behave for empty" in {
      C5Empty.filter(f) must_=== C5Empty
      C5Empty.filter(f) must_=== C5Empty
    }
  }

  "append" should {
    import Chapter3L.concat

    "behave for 1,2,3 elements" in {
      si1.append(si1).toList must_=== concat(li1,li1)
      si2.append(si1).toList must_=== concat(li2,li1)
      si3.append(si2).toList must_=== concat(li3,li2)
    }
    "behave no elements" in {
      si1.append(C5Stream.empty[Int]).toList must_=== li1
      si2.append(C5Stream.empty[Int]).toList must_=== li2
      si3.append(C5Stream.empty[Int]).toList must_=== li3
    }
    "behave for empty" in {
      C5Empty.append(si1).toList must_=== li1
      C5Empty.append(si2).toList must_=== li2
      C5Empty.append(si3).toList must_=== li3
    }
  }

  "flatMap" should {
    import Chapter3L.c3flatMap
    def fs(i: Int): C5Stream[String] = C5Cons(() => i.toString,() => C5Empty)
    def fl(i: Int): C3List[String] = C3Cons(i.toString,C3Nil)

    "behave for 1,2,3 elements" in {
      si1.flatMap(fs).toList must_=== c3flatMap(li1)(fl)
      si2.flatMap(fs).toList must_=== c3flatMap(li2)(fl)
      si3.flatMap(fs).toList must_=== c3flatMap(li3)(fl)
    }
    "behave for empty" in {
      C5Empty.flatMap(fs) must_=== C5Empty
    }
  }

  "constant" should {

    "behave for 1,2,3 elements" in {
      constant(1).take(1).toList must_=== C3Cons(1,C3Nil)
      constant(1).take(2).toList must_=== C3Cons(1,C3Cons(1,C3Nil))
      constant(1).take(3).toList must_=== C3Cons(1,C3Cons(1,C3Cons(1,C3Nil)))
    }
  }

  "from" should {

    "behave for 1,2,3 elements" in {
      from(1).take(1).toList must_=== C3Cons(1,C3Nil)
      from(1).take(2).toList must_=== C3Cons(1,C3Cons(2,C3Nil))
      from(1).take(3).toList must_=== C3Cons(1,C3Cons(2,C3Cons(3,C3Nil)))
    }
  }

  "fibNext" should {

    "behave for 1,2,3, 6 elements" in {
      // (0) 1 1 2 3 5 ... f(n+1) = f(n) + f(n-1)
      fibSeqFrom1.headOption must_=== C4Some(1)
      fibSeqFrom1.drop(1).headOption must_=== C4Some(1)
      fibSeqFrom1.drop(2).headOption must_=== C4Some(2)
      fibSeqFrom1.drop(3).headOption must_=== C4Some(3)
      fibSeqFrom1.drop(4).headOption must_=== C4Some(5)
    }
  }

  "unfoldOnes" should {
    "persevere being 1" in {
      unfoldOnes.take(0) must_=== C5Empty
      unfoldOnes.take(1).headOption must_=== C4Some(1)
      unfoldOnes.take(2).toList must_=== C3Cons(1, C3Cons(1, C3Nil))
      unfoldOnes.take(10).headOption must_=== C4Some(1)
    }
  }

  "unfoldConst" should {
    val k = "foo"
    "persevere being const" in {
      unfoldConst(k).take(0) must_=== C5Empty
      unfoldConst(k).take(1).headOption must_=== C4Some(k)
      unfoldConst(k).take(2).toList must_=== C3Cons(k, C3Cons(k, C3Nil))
      unfoldConst(k).take(10).headOption must_=== C4Some(k)
    }
  }

  "unfoldFrom" should {

    "behave for 1,2,3 elements" in {
      unfoldFrom(1).take(1).toList must_=== C3Cons(1,C3Nil)
      unfoldFrom(1).take(2).toList must_=== C3Cons(1,C3Cons(2,C3Nil))
      unfoldFrom(1).take(3).toList must_=== C3Cons(1,C3Cons(2,C3Cons(3,C3Nil)))
    }
  }

  "unfoldFib" should {

    "behave for 1,2,3, 6 elements" in {
      // (0) (1) 1 2 3 5 ... f(n+1) = f(n) + f(n-1)
      unfoldFibFrom2.headOption must_=== C4Some(1)
      unfoldFibFrom2.drop(1).headOption must_=== C4Some(2)
      unfoldFibFrom2.drop(2).headOption must_=== C4Some(3)
      unfoldFibFrom2.drop(3).headOption must_=== C4Some(5)
      unfoldFibFrom2.drop(4).headOption must_=== C4Some(8)
    }
  }

  "unfoldMap" should {
    import Chapter3L.c3map
    def f(i: Int): String = i.toString

    "behave for 1,2,3 elements" in {
      si1.unfoldMap(f).toList must_=== c3map(li1)(f)
      si2.unfoldMap(f).toList must_=== c3map(li2)(f)
      si3.unfoldMap(f).toList must_=== c3map(li3)(f)
    }
    "behave for empty" in {
      C5Empty.unfoldMap(f) must_=== C5Empty
    }
  }

  "unfoldTakeWhile" should {

    "behave for 2 elements" in {
      val sample: C3List[Int] = si3.unfoldTakeWhile(a => a <= 2).toList
      sample must_=== C3Cons(1, C3Cons(2, C3Nil))
    }
    "behave for empty" in {
      val e = C5Empty:C5Stream[Int]
      e.unfoldTakeWhile(_ => true) must_=== C5Empty
    }
  }

  "unfoldTake" should {

    "behave for 1,2,3 elements" in {
      si1.unfoldTake(1).toList must_=== li1
      si2.unfoldTake(2).toList must_=== li2
      si3.unfoldTake(3).toList must_=== li3
    }
    "behave for empty" in {
      C5Empty.unfoldTake(0) must_=== C5Empty
    }
    "behave for large n" in {
      si2.unfoldTake(20).toList must_=== li2
    }

    "behave for partial n" in {
      si2.unfoldTake(1).toList must_=== C3Cons(2, C3Nil)
      si3.unfoldTake(1).toList must_=== C3Cons(1, C3Nil)
    }
  }



}