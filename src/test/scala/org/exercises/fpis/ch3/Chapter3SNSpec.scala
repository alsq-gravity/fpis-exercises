package org.exercises.fpis.ch3

import org.specs2.mutable

object Chapter3SNSpec extends mutable.Specification {

  import Chapter3SN._
  
  def t1p: C3SNTree[Int] = C3SNStump
  def t1(a: Int): C3SNTree[Int] = C3SNNode(a,C3SNStump,C3SNStump)
  def t2p(a: Int,b:Int): C3SNTree[Int] = C3SNNode(a,C3SNNode(b,C3SNStump,C3SNStump),C3SNStump)
  def t2(a: Int, b:Int, c:Int): C3SNTree[Int] = C3SNNode(a,C3SNNode(b,C3SNStump,C3SNStump),C3SNNode(c,C3SNStump,C3SNStump))
  def t3p(a: Int, b:Int, c:Int, d:Int): C3SNTree[Int] = C3SNNode(a,C3SNNode(b,C3SNStump,C3SNNode(c,C3SNStump,C3SNStump)),C3SNNode(d,C3SNStump,C3SNStump))
  def lPop(a: Int, r: C3SNTree[Int]): C3SNTree[Int] = C3SNNode(a,C3SNStump,r)
  def rPop(l: C3SNTree[Int], a: Int): C3SNTree[Int] = C3SNNode(a,l,C3SNStump)
  def t3pa[A](a: A, b:A, c:A, d:A): C3SNTree[A] = C3SNNode(a,C3SNNode(b,C3SNStump,C3SNNode(c,C3SNStump,C3SNStump)),C3SNNode(d,C3SNStump,C3SNStump))

  "c3size" should {
    "return 0 for partial depth-one" in {
      c3size(t1p) must_=== 0
    }
    "return 1 for depth-one" in {
      c3size(t1(1)) must_=== 1
    }
    "return 3 for depth-two" in {
      c3size(t2(1,2,3)) must_=== 3
    }
    "return 4 for partial depth-three" in {
      c3size(t3p(1,2,3,4)) must_=== 4
    }
    "return 5 for partial depth-two" in {
      c3size(lPop(0,t3p(1,2,3,4))) must_=== 5
    }
  }

  "tMaxInt" should {
    "return Min for partial depth-one" in {
      tMaxInt(t1p) must_=== Int.MinValue
    }
    "return 1 for depth-one" in {
      tMaxInt(t1(1)) must_=== 1
    }
    "return 12 for depth-two" in {
      tMaxInt(t2(12,2,5)) must_=== 12
    }
    "return 15 for partial depth-three" in {
      tMaxInt(t3p(15,2,3,12)) must_=== 15
    }
  }

  "c3depth" should {
    "return depthOffset for partial depth-one" in {
      c3depth(t1p) must_=== depthOffset
    }
    "return 1 for depth-one" in {
      c3depth(t1(1)) must_=== 1 + depthOffset
    }
    "return 2 for depth-two" in {
      c3depth(t2(12,2,4)) must_=== 2 + depthOffset
    }
    "return 2 for partial depth-two" in {
      c3depth(t2p(15,2)) must_=== 2 + depthOffset
    }
    "return 3 for depth-three partial" in {
      c3depth(lPop(1,t2(3,4,5))) must_=== 3 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depth(t3p(1,2,3,4)) must_=== 3 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depth(lPop(1,t2(3,4,5))) must_=== 3 + depthOffset
    }
  }

  "c3map" should {

    def f(i: Int): Int = i*i

    "return NOP for empty" in {
      c3map(t1p)(f) must_=== t1p
    }
    "return root for root" in {
      c3map(t1(5))(f) must_=== t1(f(5))
    }
    "return partial depth-two for partial depth-two" in {
      c3map(t2p(1,2))(f) must_=== t2p(f(1),f(2))
    }
    "return depth-two for depth-two" in {
      c3map(t2(12,2,6))(f) must_=== t2(f(12),f(2),f(6))
    }
    "return sparse depth-three for sparse depth-three" in {
      c3map(t3p(-1,2,3,27))(f) must_=== t3p(f(-1),f(2),f(3),f(27))
    }
    "return partial depth-four for partial depth-four" in {
      c3map(lPop(2,t3p(4,5,6,7)))(f) must_=== lPop(f(2),t3p(f(4),f(5),f(6),f(7)))
    }
  }

  "c3sizeByF" should {
    "return 0 for partial depth-one" in {
      c3sizeByF(t1p) must_=== 0
    }
    "return 1 for depth-one" in {
      c3sizeByF(t1(1)) must_=== 1
    }
    "return 3 for depth-two" in {
      c3sizeByF(t2(1,2,3)) must_=== 3
    }
    "return 4 for partial depth-three" in {
      c3sizeByF(t3p(1,2,3,4)) must_=== 4
    }
    "return 5 for partial depth-two" in {
      c3sizeByF(lPop(0,t3p(1,2,3,4))) must_=== 5
    }
  }

  "tMaxIntByF" should {
    "return Min for partial depth-one" in {
      tMaxIntByF(t1p) must_=== Int.MinValue
    }
    "return 1 for depth-one" in {
      tMaxIntByF(t1(1)) must_=== 1
    }
    "return 12 for depth-two" in {
      tMaxIntByF(t2(12,2,5)) must_=== 12
    }
    "return 15 for partial depth-three" in {
      tMaxIntByF(t3p(15,2,3,12)) must_=== 15
    }
  }

  "c3depthByF" should {
    "return depthOffset for partial depth-one" in {
      c3depthByF(t1p) must_=== depthOffset
    }
    "return 1 for depth-one" in {
      c3depthByF(t1(1)) must_=== 1 + depthOffset
    }
    "return 2 for depth-two" in {
      c3depthByF(t2(12,2,4)) must_=== 2 + depthOffset
    }
    "return 2 for partial depth-two" in {
      c3depthByF(t2p(15,2)) must_=== 2 + depthOffset
    }
    "return 3 for depth-three partial" in {
      c3depthByF(lPop(1,t2(3,4,5))) must_=== 3 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depthByF(t3p(1,2,3,4)) must_=== 3 + depthOffset
    }
    "return 3 for partial depth-three" in {
      c3depthByF(lPop(1,t2(3,4,5))) must_=== 3 + depthOffset
    }
  }

  "c3mapByF" should {

    def f(i: Int): Int = i*i

    "return NOP for empty" in {
      c3mapByF(t1p)(f) must_=== t1p
    }
    "return root for root" in {
      c3mapByF(t1(5))(f) must_=== t1(f(5))
    }
    "return partial depth-two for partial depth-two" in {
      c3mapByF(t2p(1,2))(f) must_=== t2p(f(1),f(2))
    }
    "return depth-two for depth-two" in {
      c3mapByF(t2(12,2,6))(f) must_=== t2(f(12),f(2),f(6))
    }
    "return sparse depth-three for sparse depth-three" in {
      c3mapByF(t3p(-1,2,3,27))(f) must_=== t3p(f(-1),f(2),f(3),f(27))
    }
    "return partial depth-four for partial depth-four" in {
      c3mapByF(lPop(2,t3p(4,5,6,7)))(f) must_=== lPop(f(2),t3p(f(4),f(5),f(6),f(7)))
    }
  }

  "fold" should {
    def f(i: Int): String = (i*i).toString
    def g: C3SNTree[String] = C3SNStump
    "map a tree to a different type via f" in {
      fold[Int,String,C3SNTree[String]](t3p(1,2,3,4))(f)(g)((v:String, l:C3SNTree[String], r:C3SNTree[String]) => C3SNNode(v,l,r)) must_=== t3pa(f(1),f(2),f(3),f(4))
    }
  }

  /*

              1
             / \
            /   \
           /     \
          2       3
         / \     /
        4   5   6
       /       / \
      7       8   9

     preorder:    1 2 4 7 5 3 6 8 9
     inorder:     7 4 2 5 1 8 6 9 3
     postorder:   7 4 5 2 8 9 6 3 1
     level-order: 1 2 3 4 5 6 7 8 9

   */

  val testTree = C3SNNode(1,C3SNNode(2,C3SNNode(4,C3SNNode(7,C3SNStump,C3SNStump),C3SNStump),C3SNNode(5,C3SNStump,C3SNStump) ),C3SNNode(3,t2(6,8,9),C3SNStump))
  val preorderRes     = C3Cons(1,C3Cons(2,C3Cons(4,C3Cons(7,C3Cons(5,C3Cons(3,C3Cons(6,C3Cons(8,C3Cons(9,C3Nil)))))))))
  val inorderRes      = C3Cons(7,C3Cons(4,C3Cons(2,C3Cons(5,C3Cons(1,C3Cons(8,C3Cons(6,C3Cons(9,C3Cons(3,C3Nil)))))))))
  val postorderRes    = C3Cons(7,C3Cons(4,C3Cons(5,C3Cons(2,C3Cons(8,C3Cons(9,C3Cons(6,C3Cons(3,C3Cons(1,C3Nil)))))))))
  val breadthfirstRes = C3Cons(1,C3Cons(2,C3Cons(3,C3Cons(4,C3Cons(5,C3Cons(6,C3Cons(7,C3Cons(8,C3Cons(9,C3Nil)))))))))

  "traversals" should {
    "for preOrder, return leaves as in preorder" in {
      preOrder[Int](testTree) must_=== preorderRes
    }
  }

}
