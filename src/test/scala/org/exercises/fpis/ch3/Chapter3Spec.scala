package org.exercises.fpis.ch3

import org.specs2.mutable

object Chapter3Spec extends mutable.Specification {

  import Chapter3._

  "tail" should {
    "return Nil for empty input" in {
      tail(C3Nil) must_=== C3Nil
    }
    "return nil for list-of-one" in {
      tail(C3Cons(0, C3Nil)) must_=== C3Nil
    }
    "return list-of-one for list-of-two" in {
      tail(C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(1, C3Nil)
    }
    "return list-of-two for list-of-three" in {
      tail(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(1, C3Cons(2, C3Nil))
    }
    "return list-of-three for list-of-four" in {
      tail(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))
    }
  }

  "setHead" should {

    val head = 22

    "return Nil for empty input" in {
      setHead(head, C3Nil) must_=== C3Nil
    }
    "return modified list-of-one for list-of-one" in {
      setHead(head, C3Cons(0, C3Nil)) must_=== C3Cons(head, C3Nil)
    }
    "return modified list-of-three for list-of-three" in {
      setHead(head, C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(head, C3Cons(1, C3Cons(2, C3Nil)))
    }
  }

  "drop" should {

    val many = 200

    "return Nil for empty input" in {
      drop(many, C3Nil) must_=== C3Nil
    }
    "return nil for 1 drop on list-of-one" in {
      drop(1, C3Cons(0, C3Nil)) must_=== C3Nil
    }
    "return list-of-one for no drop on list-of-one" in {
      drop(0, C3Cons(0, C3Nil)) must_=== C3Cons(0, C3Nil)
    }
    "return list-of-one for 1 drop on list-of-two" in {
      drop(1, C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(1, C3Nil)
    }
    "return list-of-one for 2 drop on list-of-three" in {
      drop(2, C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(2, C3Nil)
    }
    "return nil for drop 4 on list-of-four" in {
      drop(4, C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== C3Nil
    }
    "return nil for drop many on list-of-four" in {
      drop(many, C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== C3Nil
    }
    "return all for drop negative on list-of-four" in {
      drop(-1, C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))
    }
  }

  "dropWhile" should {

    val test: Int => Int => Boolean = (max:Int) => (a:Int) => a < max

    "return Nil for empty input" in {
      dropWhile(C3Nil,test(0)) must_=== C3Nil
    }
    "return nil for 1 drop on list-of-one" in {
      dropWhile(C3Cons(0, C3Nil), test(1)) must_=== C3Nil
    }
    "return list-of-one for no dropWhile on list-of-one" in {
      dropWhile(C3Cons(0, C3Nil), test(-1)) must_=== C3Cons(0, C3Nil)
    }
    "return list-of-one for 1 dropWhile on list-of-two" in {
      dropWhile(C3Cons(0, C3Cons(1, C3Nil)),test(1)) must_=== C3Cons(1, C3Nil)
    }
    "return list-of-one for 2 dropWhile on list-of-three" in {
      dropWhile(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil))),test(2)) must_=== C3Cons(2, C3Nil)
    }
    "return nil for dropWhile 4 on list-of-four" in {
      dropWhile(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))), test(4)) must_=== C3Nil
    }
    "return nil for dropWhile many on list-of-four" in {
      dropWhile(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))),test(10)) must_=== C3Nil
    }
    "return list-of-three for dropWhile 3 on list-of-four" in {
      dropWhile(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))),test(1)) must_=== C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))
    }
  }

  "reverse" should {

    "return Nil for empty input" in {
      reverse(C3Nil) must_=== C3Nil
    }
    "return list-of-one for reverse on list-of-one" in {
      reverse(C3Cons(0, C3Nil)) must_=== C3Cons(0, C3Nil)
    }
    "return list-of-two reverse on list-of-two" in {
      reverse(C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(1, C3Cons(0, C3Nil))
    }
    "return list-of-three for reverse on list-of-three" in {
      reverse(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(2, C3Cons(1, C3Cons(0, C3Nil)))
    }
  }

  "init" should {
    "return Nil for empty input" in {
      init(C3Nil) must_=== C3Nil
    }
    "return Nil for list-of-one" in {
      init(C3Cons(0, C3Nil)) must_=== C3Nil
    }
    "return list-of-one for list-of-two" in {
      init(C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(0, C3Nil)
    }
    "return list-of-two for list-of-three" in {
      init(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(0, C3Cons(1, C3Nil))
    }
    "return list-of-three for list-of-four" in {
      init(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))
    }
  }

  "init1" should {
    "return Nil for empty input" in {
      init1(C3Nil) must_=== C3Nil
    }
    "return Nil for list-of-one" in {
      init1(C3Cons(0, C3Nil)) must_=== C3Nil
    }
    "return list-of-one for list-of-two" in {
      init1(C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(0, C3Nil)
    }
    "return list-of-two for list-of-three" in {
      init1(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(0, C3Cons(1, C3Nil))
    }
    "return list-of-three for list-of-four" in {
      init1(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))
    }
  }

  "foldRight" should {
    val l = C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))
    "behave for Nil and Cons" in {
      val res = foldRight(l,C3Nil:C3List[Int])(C3Cons(_,_))
      val rs = res.toString
      println(s"$rs")
      ok
    }
  }

  "lenr" should {
    "return 0 for empty input" in {
      lenr[Int](C3Nil) must_=== 0
    }
    "return 1 for list-of-one" in {
      lenr(C3Cons(0, C3Nil)) must_=== 1
    }
    "return 2 for list-of-two" in {
      lenr(C3Cons(0, C3Cons(1, C3Nil))) must_=== 2
    }
    "return 3 for list-of-three" in {
      lenr(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== 3
    }
    "return 4 for list-of-four" in {
      lenr(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== 4
    }
  }

  "lenl" should {
    "return 0 for empty input" in {
      lenl[Int](C3Nil) must_=== 0
    }
    "return 1 for list-of-one" in {
      lenl(C3Cons(0, C3Nil)) must_=== 1
    }
    "return 2 for list-of-two" in {
      lenl(C3Cons(0, C3Cons(1, C3Nil))) must_=== 2
    }
    "return 3 for list-of-three" in {
      lenl(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== 3
    }
    "return 4 for list-of-four" in {
      lenl(C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))) must_=== 4
    }
  }

  "suml" should {
    "return 0 for empty input" in {
      suml(C3Nil) must_=== 0
    }
    "return 1 for list-of-one" in {
      suml(C3Cons(1, C3Nil)) must_=== 1
    }
    "return 3 for list-of-two" in {
      suml(C3Cons(1, C3Cons(2, C3Nil))) must_=== 3
    }
    "return 6 for list-of-three" in {
      suml(C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))) must_=== 6
    }
    "return 10 for list-of-four" in {
      suml(C3Cons(1, C3Cons(2, C3Cons(3, C3Cons(4, C3Nil))))) must_=== 10
    }
  }

  "prodl" should {
    "return 1 for empty input" in {
      prodl(C3Nil) must_=== 1
    }
    "return 1 for list-of-one" in {
      prodl(C3Cons(1, C3Nil)) must_=== 1
    }
    "return 2 for list-of-two" in {
      prodl(C3Cons(1, C3Cons(2, C3Nil))) must_=== 2
    }
    "return 6 for list-of-three" in {
      prodl(C3Cons(1, C3Cons(2, C3Cons(3, C3Nil)))) must_=== 6
    }
    "return 24 for list-of-four" in {
      prodl(C3Cons(1, C3Cons(2, C3Cons(3, C3Cons(4, C3Nil))))) must_=== 24
    }
  }

  "frevl" should {

    "return Nil for empty input" in {
      frevl(C3Nil) must_=== C3Nil
    }
    "return list-of-one for frevl on list-of-one" in {
      frevl(C3Cons(0, C3Nil)) must_=== C3Cons(0, C3Nil)
    }
    "return list-of-two frevl on list-of-two" in {
      frevl(C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(1, C3Cons(0, C3Nil))
    }
    "return list-of-three for frevl on list-of-three" in {
      frevl(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))) must_=== C3Cons(2, C3Cons(1, C3Cons(0, C3Nil)))
    }
  }

  "foldRightL" should {
    val l = C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))
    "behave for Nil and Cons" in {
      val res = foldRightL(l,C3Nil:C3List[Int])(C3Cons(_,_))
      val rs = res.toString
      println(s"$rs")
      ok
    }
  }

  "append" should {
    val item = 100
    "return list-of-one for empty input" in {
      append(C3Nil,item) must_=== C3Cons(item,C3Nil)
    }
    "return list-of-two for list-of-one" in {
      append(C3Cons(0, C3Nil),item) must_=== C3Cons(0, C3Cons(item,C3Nil))
    }
    "return list-of-three for list-of-two" in {
      append(C3Cons(0, C3Cons(1, C3Nil)),item) must_=== C3Cons(0, C3Cons(1, C3Cons(item, C3Nil)))
    }
    "return list-of-four for reverse on list-of-three" in {
      append(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil))),item) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(item, C3Nil))))
    }
  }

  "appendL" should {
    val item = 100
    "return list-of-one for empty input" in {
      appendL(C3Nil,item) must_=== C3Cons(item,C3Nil)
    }
    "return list-of-two for list-of-one" in {
      appendL(C3Cons(0, C3Nil),item) must_=== C3Cons(0, C3Cons(item,C3Nil))
    }
    "return list-of-three for list-of-two" in {
      appendL(C3Cons(0, C3Cons(1, C3Nil)),item) must_=== C3Cons(0, C3Cons(1, C3Cons(item, C3Nil)))
    }
    "return list-of-four for reverse on list-of-three" in {
      appendL(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil))),item) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(item, C3Nil))))
    }
  }
  
  "appendLL" should {
    val item = 100
    "return list-of-one for empty input" in {
      appendLL(C3Nil,item) must_=== C3Cons(item,C3Nil)
    }
    "return list-of-two for list-of-one" in {
      appendLL(C3Cons(0, C3Nil),item) must_=== C3Cons(0, C3Cons(item,C3Nil))
    }
    "return list-of-three for list-of-two" in {
      appendLL(C3Cons(0, C3Cons(1, C3Nil)),item) must_=== C3Cons(0, C3Cons(1, C3Cons(item, C3Nil)))
    }
    "return list-of-four for reverse on list-of-three" in {
      appendLL(C3Cons(0, C3Cons(1, C3Cons(2, C3Nil))),item) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(item, C3Nil))))
    }
  }

  "concat" should {
    "return list-of-one for (one,empty) input" in {
      concat(C3Cons(0,C3Nil), C3Nil) must_=== C3Cons(0,C3Nil)
    }
    "return list-of-one for (empty,one) input" in {
      concat(C3Nil, C3Cons(0,C3Nil)) must_=== C3Cons(0,C3Nil)
    }
    "return list-of-two for (onea, oneb)" in {
      concat(C3Cons(0, C3Nil),C3Cons(1, C3Nil)) must_=== C3Cons(0, C3Cons(1,C3Nil))
    }
    "return list-of-two for (oneb, onea)" in {
      concat(C3Cons(1, C3Nil),C3Cons(0, C3Nil)) must_=== C3Cons(1, C3Cons(0,C3Nil))
    }
    "return list-of-three for (two,one)" in {
      concat(C3Cons(0, C3Cons(1, C3Nil)),C3Cons(2, C3Nil)) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Nil)))
    }
    "return list-of-three for (one,two)" in {
      concat(C3Cons(2, C3Nil),C3Cons(0, C3Cons(1, C3Nil))) must_=== C3Cons(2, C3Cons(0, C3Cons(1, C3Nil)))
    }
    "return list-of-four for (two,two)" in {
      concat(C3Cons(0, C3Cons(1, C3Nil)), C3Cons(2, C3Cons(3, C3Nil))) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))
    }
  }

  "flatten" should {
    val l03 = C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Nil))))
    val l45 = C3Cons(4, C3Cons(5, C3Nil))
    val l68 = C3Cons(6, C3Cons(7, C3Cons(8, C3Nil)))
    val ll = C3Cons(l03, C3Cons(l45, C3Cons(l68, C3Nil)))
    "work" in {
      flatten(ll) must_=== C3Cons(0, C3Cons(1, C3Cons(2, C3Cons(3, C3Cons(4, C3Cons(5, C3Cons(6, C3Cons(7, C3Cons(8, C3Nil)))))))))
    }
  }

}