package org.exercises.fpis.ch3

import org.specs2.mutable

object Chapter3LBSpec extends mutable.Specification {

  import Chapter3LB._
  
  def t1p(a: Int) = C3LBLeaf(a)
  def t1(a: Int,b:Int) = C3LBBranch(C3LBLeaf(a),C3LBLeaf(b))
  def t2p(a: Int, b:Int, c:Int) = C3LBBranch(C3LBLeaf(a),t1(b,c))
  def t2(a: Int, b:Int, c:Int, d:Int) = C3LBBranch(t1(a,b),t1(c,d))
  def lPop(a: Int, r: C3LBTree[Int]) = C3LBBranch(C3LBLeaf(a),r)
  def rPop(l: C3LBTree[Int], a: Int) = C3LBBranch(l,C3LBLeaf(a))


  "c3size" should {
    "return 0 for partial depth-one" in {
      c3size(t1p(1)) must_=== 1
    }
    "return 3 for depth-one" in {
      c3size(t1(1,2)) must_=== 3
    }
    "return 5 for partial depth-two" in {
      c3size(t2p(1,2,3)) must_=== 5
    }
    "return 7 for depth-two" in {
      c3size(t2(1,2,3,4)) must_=== 7
    }
  }

  "tMaxInt" should {
    "return 1 for partial depth-one" in {
      tMaxInt(t1p(1)) must_=== 1
    }
    "return 12 for depth-one" in {
      tMaxInt(t1(12,2)) must_=== 12
    }
    "return 15 for partial depth-two" in {
      tMaxInt(t2p(15,2,3)) must_=== 15
    }
    "return 7 for depth-two" in {
      tMaxInt(t2(1,7,4,5)) must_=== 7
    }
  }

  "c3depth" should {
    "return 0 for partial depth-one" in {
      c3depth(t1p(1)) must_=== 0 + depthOffset
    }
    "return 1 for depth-one" in {
      c3depth(t1(12,2)) must_=== 1 + depthOffset
    }
    "return 2 for partial depth-two" in {
      c3depth(t2p(15,2,3)) must_=== 2 + depthOffset
    }
    "return 2 for depth-two partial" in {
      c3depth(lPop(1,t1(3,4))) must_=== 2 + depthOffset
    }
    "return 2 for depth-two" in {
      c3depth(t2(1,2,3,4)) must_=== 2 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depth(lPop(1,t2(3,4,5,6))) must_=== 3 + depthOffset
    }
  }

  "c3map" should {

    def f(i: Int): Int = i*i

    "return root for root" in {
      c3map(C3LBLeaf(5))(f) must_=== C3LBLeaf(f(5))
    }
    "return partial depth-one for partial depth-one" in {
      c3map(t1p(1))(f) must_=== t1p(f(1))
    }
    "return depth-one for depth-one" in {
      c3map(t1(12,2))(f) must_=== t1(f(12),f(2))
    }
    "return sparse depth-two for sparse depth-two" in {
      c3map(t2p(-1,2,3))(f) must_=== t2p(f(-1),f(2),f(3))
    }
    "return partial depth-two for partial depth-two" in {
      c3map(t2p(15,2,3))(f) must_=== t2p(f(15),f(2),f(3))
    }
    "return depth-two for depth-two" in {
      c3map(t2(1,2,3,4))(f) must_=== t2(f(1),f(2),f(3),f(4))
    }
    "return partial depth-three for partial depth-three" in {
      c3map(lPop(2,t2(4,5,6,7)))(f) must_=== lPop(f(2),t2(f(4),f(5),f(6),f(7)))
    }
  }

  "c3sizeByF" should {
    "return 0 for partial depth-one" in {
      c3sizeByF(t1p(1)) must_=== 1
    }
    "return 3 for depth-one" in {
      c3sizeByF(t1(1,2)) must_=== 3
    }
    "return 5 for partial depth-two" in {
      c3sizeByF(t2p(1,2,3)) must_=== 5
    }
    "return 7 for depth-two" in {
      c3sizeByF(t2(1,2,3,4)) must_=== 7
    }
  }

  "tMaxIntByF" should {
    "return 1 for partial depth-one" in {
      tMaxIntByF(t1p(1)) must_=== 1
    }
    "return 12 for depth-one" in {
      tMaxIntByF(t1(12,2)) must_=== 12
    }
    "return 15 for partial depth-two" in {
      tMaxIntByF(t2p(15,2,3)) must_=== 15
    }
    "return 7 for depth-two" in {
      tMaxIntByF(t2(1,7,4,5)) must_=== 7
    }
  }

  "c3depthByF" should {
    "return 0 for partial depth-one" in {
      c3depthByF(t1p(1)) must_=== 0 + depthOffset
    }
    "return 1 for depth-one" in {
      c3depthByF(t1(12,2)) must_=== 1 + depthOffset
    }
    "return 2 for partial depth-two" in {
      c3depthByF(t2p(15,2,3)) must_=== 2 + depthOffset
    }
    "return 2 for depth-two partial" in {
      c3depthByF(lPop(1,t1(3,4))) must_=== 2 + depthOffset
    }
    "return 2 for depth-two" in {
      c3depthByF(t2(1,2,3,4)) must_=== 2 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depthByF(lPop(1,t2(3,4,5,6))) must_=== 3 + depthOffset
    }
  }

  "c3mapByF" should {

    def f(i: Int): Int = i*i

    "return root for root" in {
      c3mapByF(C3LBLeaf(5))(f) must_=== C3LBLeaf(f(5))
    }
    "return partial depth-one for partial depth-one" in {
      c3mapByF(t1p(1))(f) must_=== t1p(f(1))
    }
    "return depth-one for depth-one" in {
      c3mapByF(t1(12,2))(f) must_=== t1(f(12),f(2))
    }
    "return sparse depth-two for sparse depth-two" in {
      c3mapByF(t2p(-1,2,3))(f) must_=== t2p(f(-1),f(2),f(3))
    }
    "return partial depth-two for partial depth-two" in {
      c3mapByF(t2p(15,2,3))(f) must_=== t2p(f(15),f(2),f(3))
    }
    "return depth-two for depth-two" in {
      c3mapByF(t2(1,2,3,4))(f) must_=== t2(f(1),f(2),f(3),f(4))
    }
    "return partial depth-three for partial depth-three" in {
      c3mapByF(lPop(2,t2(4,5,6,7)))(f) must_=== lPop(f(2),t2(f(4),f(5),f(6),f(7)))
    }
  }

  "leftLinearMap" should {

    def f(i: Int): C3LBTree[Int] = C3LBLeaf(i)

    "return depth-zero for depth-zero" in {
      leftLinearMap(C3LBLeaf(5))(f) must_=== C3Cons(f(5),C3Nil)
    }
    "return depth-one for depth-one" in {
      leftLinearMap[Int,C3LBTree[Int]](t1(12,2))(f) must_=== C3Cons(f(12), C3Cons(f(2), C3Nil))
    }
    "return partial depth-two for partial depth-two" in {
      leftLinearMap(t2p(15,2,3))(f) must_=== C3Cons(f(15), C3Cons(f(2), C3Cons(f(3), C3Nil)))
    }
    "return depth-two for depth-two" in {
      leftLinearMap[Int,C3LBTree[Int]](lPop(1,t1(2,3)))(f) must_=== C3Cons(f(1), C3Cons(f(2), C3Cons(f(3), C3Nil)))
    }
    "return partial depth-three for partial depth-three" in {
      leftLinearMap(lPop(1,rPop(t2(4,5,6,7),8)))(f) must_=== C3Cons(f(1), C3Cons(f(4), C3Cons(f(5), C3Cons(f(6), C3Cons(f(7), C3Cons(f(8), C3Nil))))))
    }
  }

  "deepLeft" should {

    def f(i: Int): C3LBLeaf[Int] = C3LBLeaf(i)

    "return depth-zero for depth-zero" in {
      val res: C3LBTree[Int] = f(5)
      deepLeft(C3Cons(f(5),C3Nil)) must_=== res
    }
    "return depth-one for depth-one" in {
      deepLeft(C3Cons(f(12), C3Cons(f(2), C3Nil))) must_=== t1(12,2)
    }
    "return partial depth-two for partial depth-two" in {
      deepLeft(C3Cons(f(15), C3Cons(f(2), C3Cons(f(3), C3Nil)))) must_=== rPop(t1(15,2),3)
    }
    "return depth-two for depth-two" in {
      deepLeft(C3Cons(f(1), C3Cons(f(2), C3Cons(f(3), C3Cons(f(4), C3Nil))))) must_=== t2(1,2,3,4)
    }
    "return partial depth-three for partial depth-three" in {
      deepLeft(C3Cons(f(1), C3Cons(f(4), C3Cons(f(5), C3Cons(f(6), C3Cons(f(7), C3Cons(f(8), C3Nil))))))) must_=== C3LBBranch(t2(1,4,5,6),t1(7,8))
    }
  }

  "deepLeftMap" should {

    "return depth-zero for depth-zero" in {
      val res: C3LBTree[Int] = C3LBLeaf(5)
      deepLeftMap[Int,Int](C3LBLeaf(5))(identity) must_=== res
    }
    "return depth-one for depth-one" in {
      deepLeftMap[Int,Int](t1(12,2))(identity) must_=== t1(12,2)
    }
    "return partial depth-two for partial depth-two" in {
      deepLeftMap[Int,Int](t2p(15,2,3))(identity) must_=== rPop(t1(15,2),3)
    }
    "return depth-two for depth-two" in {
      deepLeftMap[Int,Int](lPop(1,t1(2,3)))(identity) must_=== rPop(t1(1,2),3)
    }
    "return partial depth-three for partial depth-three" in {
      deepLeftMap[Int,Int](lPop(1,rPop(t2(4,5,6,7),8)))(identity) must_=== C3LBBranch(t2(1,4,5,6),t1(7,8))
    }
  }

}
