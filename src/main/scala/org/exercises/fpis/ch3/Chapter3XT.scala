package org.exercises.fpis.ch3

import org.exercises.fpis.ch3.Chapter3L.C3Stack

import scala.annotation.tailrec

object Chapter3XT {

  val depthOffset = 0 // if 0, root has depth 0 (same offset style as for arrays)

  // 3.25
  def c3size[A](t: C3XTree[A]): Int = {

    def go(st: C3XTree[A], count: Int): Int = st match {
      case C3XLeaf(_) | C3XStump => count + 1
      case C3XBranch(l, r) => count+1 + go(l, 0) + go(r, 0)
    }
    go(t,0)
  }

  // 3.26
  def tMaxInt(t: C3XTree[Int]): Int = {

    def go(st: C3XTree[Int], cMax: Int): Int = st match {
      case C3XLeaf(v) => Math.max(cMax,v)
      case C3XStump => cMax
      case C3XBranch(l, r) => Math.max(cMax, Math.max(go(l, Int.MinValue), go(r, Int.MinValue)))
    }
    go(t,Int.MinValue)
  }

  // 3.27
  def c3depth[A](t: C3XTree[A]): Int = {

    { require (0 == depthOffset || 1 == depthOffset) }

    def go(st: C3XTree[A], depth: Int): Int = st match {
      case C3XLeaf(_) | C3XStump => depth + depthOffset
      case C3XBranch(l, r) => Math.max(go(l, depth+1), go(r, depth+1))
    }
    go(t,0)
  }

  // 3.28
  def c3map[A,B](in: C3XTree[A])(f: A => B): C3XTree[B] = in match {
    case C3XLeaf(v) => C3XLeaf(f(v))
    case C3XStump => C3XStump
    case C3XBranch(l, r) => C3XBranch(c3map(l)(f),c3map(r)(f))
  }

  // 3.29 (depth first)
  def fold[A,B](t: C3XTree[A])(f: A => B)(sCons: => B)(bCons: (B, B) => B): B = t match {
    case C3XStump => sCons
    case C3XLeaf(a) => f(a)
    case C3XBranch(l,r) => bCons(fold(l)(f)(sCons)(bCons),fold(r)(f)(sCons)(bCons))
  }

  def c3sizeByF[A](t: C3XTree[A]): Int =
    fold[A,Int](t)((_:A) => 1)(1)((lCount:Int, rCount:Int) => lCount+rCount+1)
  def tMaxIntByF(t: C3XTree[Int]): Int =
    fold[Int,Int](t)((v:Int) => v)(Int.MinValue)((lMax:Int, rMax:Int) => Math.max(lMax,rMax))
  def c3depthByF[A](t: C3XTree[A]): Int = {
    { require (0 == depthOffset || 1 == depthOffset) }
    fold[A,Int](t)((_:A) => depthOffset)(depthOffset)((lDepth:Int, rDepth:Int) => Math.max(lDepth+1,rDepth+1))
  }
  def c3mapByF[A,B](in: C3XTree[A])(f: A => B): C3XTree[B] =
    fold[A,C3XTree[B]](in)((a:A) => C3XLeaf(f(a)))(C3XStump:C3XTree[B])((l:C3XTree[B], r:C3XTree[B]) => C3XBranch(l,r))

  // not requested

  def leftLinear[A](t: C3XTree[A]): C3Stack[C3XTree[A]] = {
    import Chapter3L.{push,pop,isEmpty}

    @tailrec
    def pushTree(tree: C3XTree[A], stack: C3Stack[C3XTree[A]], acc: C3Stack[C3XTree[A]]): C3Stack[C3XTree[A]] = tree match {
      case C3XStump | C3XLeaf(_) =>  /* tree -> acc */ popLeaf(stack, push(tree, acc))
      case C3XBranch(l,r) => /* tree -> stack */ pushTree(r,push(l,stack),acc)
    }

    @tailrec
    def popLeaf(stack: C3Stack[C3XTree[A]], acc: C3Stack[C3XTree[A]]): C3Stack[C3XTree[A]] = stack match {
      case _ if isEmpty(stack) => acc
      case _ =>
      val (tr,trs) = pop(stack)
      tr match {
        case C3XBranch(_,_) => /* stack -> tree */ pushTree(tr,trs,acc)
        case C3XStump | C3XLeaf(_) =>  /* stack -> acc */ popLeaf(trs, push(tr, acc))
      }
    }

    pushTree(t,C3Nil,C3Nil)
  }

}
