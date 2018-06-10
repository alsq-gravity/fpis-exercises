package org.exercises.fpis.ch3


object Chapter3SN {

  val depthOffset = 0 // if 0, root has depth 0 (same offset style as for arrays)

  // 3.25
  def c3size[A](t: C3SNTree[A]): Int = {

    def go(st: C3SNTree[A], count: Int): Int = st match {
      case C3SNStump => count
      case C3SNNode(_, l, r) => count+1 + go(l, 0) + go(r, 0)
    }
    go(t,0)
  }

  // 3.26
  def tMaxInt(t: C3SNTree[Int]): Int = {

    def go(st: C3SNTree[Int], cMax: Int): Int = st match {
      case C3SNStump => cMax
      case C3SNNode(v, l, r) => Math.max(Math.max(v, cMax), Math.max(go(l, Int.MinValue), go(r, Int.MinValue)))
    }
    go(t,Int.MinValue)
  }

  // 3.27
  def c3depth[A](t: C3SNTree[A]): Int = {

    { require (0 == depthOffset || 1 == depthOffset) }

    def go(st: C3SNTree[A], depth: Int): Int = st match {
      case C3SNStump => depth + depthOffset
      case C3SNNode(_, l, r) => Math.max(go(l, depth+1), go(r, depth+1))
    }
    go(t,0)
  }

  // 3.28
  def c3map[A,B](in: C3SNTree[A])(f: A => B): C3SNTree[B] = in match {
    case C3SNStump => C3SNStump
    case C3SNNode(v, l, r) => C3SNNode(f(v),c3map(l)(f), c3map(r)(f))
  }

  // 3.29
  def fold[A,B,C](t: C3SNTree[A])(f: A => B)(sCons: => C)(bCons: (B, C, C) => C): C = t match {
    case C3SNStump => sCons
    case C3SNNode(v,l,r) => bCons(f(v), fold(l)(f)(sCons)(bCons),fold(r)(f)(sCons)(bCons))
  }

  def c3sizeByF[A,B](t: C3SNTree[A]): Int =
    fold[A,Int,Int](t)((_:A) => 1)(0)((_:Int, lCount:Int, rCount:Int) => lCount+rCount+1)
  def tMaxIntByF[A](t: C3SNTree[Int]): Int =
    fold[Int,Int,Int](t)((v:Int) => v)(Int.MinValue)((v:Int, lMax:Int, rMax:Int) => Math.max(v,Math.max(lMax,rMax)))
  def c3depthByF[A,B](t: C3SNTree[A]): Int = {
    { require (0 == depthOffset || 1 == depthOffset) }
    fold[A,Int,Int](t)((_:A) => depthOffset)(0)((_:Int, lDepth:Int, rDepth:Int) => Math.max(lDepth+1,rDepth+1))
  }
  def c3mapByF[A,B](in: C3SNTree[A])(f: A => B): C3SNTree[B] =
    fold[A,B,C3SNTree[B]](in)(f)(C3SNStump)((v:B, l:C3SNTree[B], r:C3SNTree[B]) => C3SNNode(v,l,r))

  def preOrder[A](t: C3SNTree[A]): C3List[A] = fold[A,A,C3List[A]](t)(identity)(C3Nil)((a:A,l:C3List[A],r:C3List[A]) => C3Cons(a, Chapter3L.concat(l,r)))
}
