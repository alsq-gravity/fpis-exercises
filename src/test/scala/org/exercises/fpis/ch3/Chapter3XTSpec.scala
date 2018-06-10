package org.exercises.fpis.ch3

import org.specs2.mutable

object Chapter3XTSpec extends mutable.Specification {

  import Chapter3XT._
  
  def t1p(a: Int) = C3XBranch(C3XLeaf(a),C3XStump)
  def t1(a: Int,b:Int) = C3XBranch(C3XLeaf(a),C3XLeaf(b))
  def t2pp(a: Int, b:Int) = C3XBranch(C3XLeaf(a),t1p(b))
  def t2p(a: Int, b:Int, c:Int) = C3XBranch(C3XLeaf(a),t1(b,c))
  def lPop(a: Int, b:Int, r: C3XTree[Int]) = C3XBranch(t1(a,b),r)
  def rPop(l: C3XTree[Int], a: Int, b:Int) = C3XBranch(l,t1(a,b))

  "c3size" should {
    "return 1 for empty input" in {
      c3size[Int](C3XStump) must_=== 1
    }
    "return 3 for partial depth-one" in {
      c3size(t1p(1)) must_=== 3
    }
    "return 3 for depth-one" in {
      c3size(t1(1,2)) must_=== 3
    }
    "return 5 for sparse depth-two" in {
      c3size(t2pp(1,2)) must_=== 5
    }
    "return 5 for partial depth-two" in {
      c3size(t2p(1,2,3)) must_=== 5
    }
    "return 7 for depth-two" in {
      c3size(lPop(1,2,t1(3,4))) must_=== 7
    }
  }

  "tMaxInt" should {
    "return MIN for empty input" in {
      tMaxInt(C3XStump) must_=== Int.MinValue
    }
    "return 1 for partial depth-one" in {
      tMaxInt(t1p(1)) must_=== 1
    }
    "return 12 for depth-one" in {
      tMaxInt(t1(12,2)) must_=== 12
    }
    "return 2 for sparse depth-two" in {
      tMaxInt(t2pp(-1,2)) must_=== 2
    }
    "return 15 for partial depth-two" in {
      tMaxInt(t2p(15,2,3)) must_=== 15
    }
    "return 7 for depth-two" in {
      tMaxInt(lPop(1,2,t1(7,4))) must_=== 7
    }
  }

  "c3depth" should {
    "return 0 for empty input" in {
      c3depth(C3XStump) must_=== 0 + depthOffset
    }
    "return 1 for partial depth-one" in {
      c3depth(t1p(1)) must_=== 1 + depthOffset
    }
    "return 1 for depth-one" in {
      c3depth(t1(12,2)) must_=== 1 + depthOffset
    }
    "return 2 for sparse depth-two" in {
      c3depth(t2pp(-1,2)) must_=== 2 + depthOffset
    }
    "return 2 for partial depth-two" in {
      c3depth(t2p(15,2,3)) must_=== 2 + depthOffset
    }
    "return 2 for depth-two" in {
      c3depth(lPop(1,2,t1(3,4))) must_=== 2 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depth(lPop(1,2,rPop(t1p(4),5,6))) must_=== 3 + depthOffset
    }
  }

  "c3map" should {

    def f(i: Int): Int = i*i

    "return empty for empty input" in {
      c3map(C3XStump)(f) must_=== C3XStump
    }
    "return root for root" in {
      c3map(C3XLeaf(5))(f) must_=== C3XLeaf(f(5))
    }
    "return partial depth-one for partial depth-one" in {
      c3map(t1p(1))(f) must_=== t1p(f(1))
    }
    "return depth-one for depth-one" in {
      c3map(t1(12,2))(f) must_=== t1(f(12),f(2))
    }
    "return sparse depth-two for sparse depth-two" in {
      c3map(t2pp(-1,2))(f) must_=== t2pp(f(-1),f(2))
    }
    "return partial depth-two for partial depth-two" in {
      c3map(t2p(15,2,3))(f) must_=== t2p(f(15),f(2),f(3))
    }
    "return depth-two for depth-two" in {
      c3map(lPop(1,2,t1(3,4)))(f) must_=== lPop(f(1),f(2),t1(f(3),f(4)))
    }
    "return partial depth-three for partial depth-three" in {
      c3map(lPop(1,2,rPop(t1p(4),5,6)))(f) must_=== lPop(f(1),f(2),rPop(t1p(f(4)),f(5),f(6)))
    }
  }

  "c3sizeByF" should {
    "return 1 for empty input" in {
      c3sizeByF[Int](C3XStump) must_=== 1
    }
    "return 3 for partial depth-one" in {
      c3sizeByF(t1p(1)) must_=== 3
    }
    "return 3 for depth-one" in {
      c3sizeByF(t1(1,2)) must_=== 3
    }
    "return 5 for sparse depth-two" in {
      c3sizeByF(t2pp(1,2)) must_=== 5
    }
    "return 5 for partial depth-two" in {
      c3sizeByF(t2p(1,2,3)) must_=== 5
    }
    "return 7 for depth-two" in {
      c3sizeByF(lPop(1,2,t1(3,4))) must_=== 7
    }
  }

  "tMaxIntByF" should {
    "return MIN for empty input" in {
      tMaxIntByF(C3XStump) must_=== Int.MinValue
    }
    "return 1 for partial depth-one" in {
      tMaxIntByF(t1p(1)) must_=== 1
    }
    "return 12 for depth-one" in {
      tMaxIntByF(t1(12,2)) must_=== 12
    }
    "return 2 for sparse depth-two" in {
      tMaxIntByF(t2pp(-1,2)) must_=== 2
    }
    "return 15 for partial depth-two" in {
      tMaxIntByF(t2p(15,2,3)) must_=== 15
    }
    "return 7 for depth-two" in {
      tMaxIntByF(lPop(1,2,t1(7,4))) must_=== 7
    }
  }

  "c3depthByF" should {
    "return 0 for empty input" in {
      c3depthByF(C3XStump) must_=== 0 + depthOffset
    }
    "return 1 for partial depth-one" in {
      c3depthByF(t1p(1)) must_=== 1 + depthOffset
    }
    "return 1 for depth-one" in {
      c3depthByF(t1(12,2)) must_=== 1 + depthOffset
    }
    "return 2 for sparse depth-two" in {
      c3depthByF(t2pp(-1,2)) must_=== 2 + depthOffset
    }
    "return 2 for partial depth-two" in {
      c3depthByF(t2p(15,2,3)) must_=== 2 + depthOffset
    }
    "return 2 for depth-two" in {
      c3depthByF(lPop(1,2,t1(3,4))) must_=== 2 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depthByF(lPop(1,2,rPop(t1p(4),5,6))) must_=== 3 + depthOffset
    }
  }

  "c3mapByF" should {

    def f(i: Int): Int = i*i

    "return empty for empty input" in {
      c3mapByF(C3XStump)(f) must_=== C3XStump
    }
    "return root for root" in {
      c3mapByF(C3XLeaf(5))(f) must_=== C3XLeaf(f(5))
    }
    "return partial depth-one for partial depth-one" in {
      c3mapByF(t1p(1))(f) must_=== t1p(f(1))
    }
    "return depth-one for depth-one" in {
      c3mapByF(t1(12,2))(f) must_=== t1(f(12),f(2))
    }
    "return sparse depth-two for sparse depth-two" in {
      c3mapByF(t2pp(-1,2))(f) must_=== t2pp(f(-1),f(2))
    }
    "return partial depth-two for partial depth-two" in {
      c3mapByF(t2p(15,2,3))(f) must_=== t2p(f(15),f(2),f(3))
    }
    "return depth-two for depth-two" in {
      c3mapByF(lPop(1,2,t1(3,4)))(f) must_=== lPop(f(1),f(2),t1(f(3),f(4)))
    }
    "return partial depth-three for partial depth-three" in {
      c3mapByF(lPop(1,2,rPop(t1p(4),5,6)))(f) must_=== lPop(f(1),f(2),rPop(t1p(f(4)),f(5),f(6)))
    }
  }

  "leftLinear" should {

    def f(i: Int): C3XTree[Int] = C3XLeaf(i)

    "return empty for empty input" in {
      leftLinear(C3XStump) must_=== C3Cons(C3XStump, C3Nil)
    }
    "return root for root" in {
      leftLinear(C3XLeaf(5)) must_=== C3Cons(f(5), C3Nil)
    }
    "return partial depth-one for partial depth-one" in {
      leftLinear(t1p(1)) must_=== C3Cons(f(1), C3Cons(C3XStump, C3Nil))
    }
    "return depth-one for depth-one" in {
      leftLinear(t1(12,2)) must_=== C3Cons(f(12), C3Cons(f(2), C3Nil))
    }
    "return sparse depth-two for sparse depth-two" in {
      leftLinear(t2pp(-1,2)) must_=== C3Cons(f(-1), C3Cons(f(2), C3Cons(C3XStump, C3Nil)))
    }
    "return partial depth-two for partial depth-two" in {
      leftLinear(t2p(15,2,3)) must_=== C3Cons(f(15), C3Cons(f(2), C3Cons(f(3),C3Nil)))
    }
    "return depth-two for depth-two" in {
      leftLinear(lPop(1,2,t1(3,4))) must_=== C3Cons(f(1), C3Cons(f(2), C3Cons(f(3), C3Cons(f(4), C3Nil))))
    }
    "return partial depth-three for partial depth-three" in {
      leftLinear(lPop(1,2,rPop(t2p(4,5,6),7,8))) must_=== C3Cons(f(1), C3Cons(f(2), C3Cons(f(4), C3Cons(f(5), C3Cons(f(6), C3Cons(f(7), C3Cons(f(8), C3Nil)))))))
    }
  }

}
