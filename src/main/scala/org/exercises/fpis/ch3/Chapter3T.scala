package org.exercises.fpis.ch3

object Chapter3T {

  val depthOffset = 0 // if 0, root has depth 0 (same offset style as for arrays)

  // 3.25
  def c3size[A](t: C3Tree[A]): Int = {

    def go(st: C3Tree[A], count: Int): Int = st match {
      case C3Leaf(_) | C3Stump => count + 1
      case C3Branch(l, r) => count+1 + go(l, 0) + go(r, 0)
    }
    go(t,0)
  }

  // 3.26
  def tMaxInt(t: C3Tree[Int]): Int = {

    def go(st: C3Tree[Int], cMax: Int): Int = st match {
      case C3Leaf(v) => Math.max(cMax,v)
      case C3Stump => cMax
      case C3Branch(l, r) => Math.max(cMax, Math.max(go(l, Int.MinValue), go(r, Int.MinValue)))
    }
    go(t,Int.MinValue)
  }

  // 3.27
  def c3depth[A](t: C3Tree[A]): Int = {

    { require (0 == depthOffset || 1 == depthOffset) }

    def go(st: C3Tree[A], depth: Int): Int = st match {
      case C3Leaf(_) | C3Stump => depth + depthOffset
      case C3Branch(l, r) => Math.max(go(l, depth+1), go(r, depth+1))
    }
    go(t,0)
  }

  // 3.28
  def c3map[A,B](in: C3Tree[A])(f: A => B): C3Tree[B] = in match {
    case C3Leaf(v) => C3Leaf(f(v))
    case C3Stump => C3Stump
    case C3Branch(l, r) => C3Branch(c3map(l)(f),c3map(r)(f))
  }

  // not requested

  def push[A](v: A, stack:C3List[A]):C3List[A] = C3Cons(v,stack)
  def isEmpty[A](stack:C3List[A]): Boolean = stack match {
    case C3Nil => true
    case _ => false
  }
  def pop[A](stack:C3List[A]): (A,C3List[A])  = stack match {
    case C3Cons(x,xs) => (x,xs)
    case _ => throw new RuntimeException("call isEmpty first")
  }


  // 3.29
  def fold[A,B](t: C3Tree[A])(f: A => B)(sCons: => B)(bCons: (B, B) => B): B = t match {
    case C3Stump => sCons
    case C3Leaf(a) => f(a)
    case C3Branch(l,r) => bCons(fold(l)(f)(sCons)(bCons),fold(r)(f)(sCons)(bCons))
  }

  def c3sizeByF[A](t: C3Tree[A]): Int =
    fold[A,Int](t)((_:A) => 1)(1)((lCount:Int, rCount:Int) => lCount+rCount+1)
  def tMaxIntByF(t: C3Tree[Int]): Int =
    fold[Int,Int](t)((v:Int) => v)(Int.MinValue)((lMax:Int, rMax:Int) => Math.max(lMax,rMax))
  def c3depthByF[A](t: C3Tree[A]): Int = {
    { require (0 == depthOffset || 1 == depthOffset) }
    fold[A,Int](t)((_:A) => depthOffset)(depthOffset)((lDepth:Int, rDepth:Int) => Math.max(lDepth+1,rDepth+1))
  }
  def c3mapByF[A,B](in: C3Tree[A])(f: A => B): C3Tree[B] =
    fold[A,C3Tree[B]](in)((a:A) => C3Leaf(f(a)))(C3Stump:C3Tree[B])((l:C3Tree[B],r:C3Tree[B]) => C3Branch(l,r))
}
