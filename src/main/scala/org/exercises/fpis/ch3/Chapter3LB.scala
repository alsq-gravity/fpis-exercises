package org.exercises.fpis.ch3

import org.exercises.fpis.ch3.Chapter3L.C3Stack

import scala.annotation.tailrec

object Chapter3LB {

  val depthOffset = 0 // if 0, root has depth 0 (same offset style as for arrays)

  // 3.25
  def c3size[A](t: C3LBTree[A]): Int = {

    def go(st: C3LBTree[A], count: Int): Int = st match {
      case C3LBLeaf(_) => count + 1
      case C3LBBranch(l, r) => count+1 + go(l, 0) + go(r, 0)
    }
    go(t,0)
  }

  // 3.26
  def tMaxInt(t: C3LBTree[Int]): Int = {

    def go(st: C3LBTree[Int], cMax: Int): Int = st match {
      case C3LBLeaf(v) => Math.max(cMax,v)
      case C3LBBranch(l, r) => Math.max(cMax, Math.max(go(l, Int.MinValue), go(r, Int.MinValue)))
    }
    go(t,Int.MinValue)
  }

  // 3.27
  def c3depth[A](t: C3LBTree[A]): Int = {

    { require (0 == depthOffset || 1 == depthOffset) }

    def go(st: C3LBTree[A], depth: Int): Int = st match {
      case C3LBLeaf(_) => depth + depthOffset
      case C3LBBranch(l, r) => Math.max(go(l, depth+1), go(r, depth+1))
    }
    go(t,0)
  }

  // 3.28
  def c3map[A,B](in: C3LBTree[A])(f: A => B): C3LBTree[B] = in match {
    case C3LBLeaf(v) => C3LBLeaf(f(v))
    case C3LBBranch(l, r) => C3LBBranch(c3map(l)(f),c3map(r)(f))
  }

  // 3.29 (depth first)
  def fold[A,B](t: C3LBTree[A])(f: A => B)(bCons: (B, B) => B): B = t match {
    case C3LBLeaf(a) => f(a)
    case C3LBBranch(l,r) => bCons(fold(l)(f)(bCons),fold(r)(f)(bCons))
  }

  def c3sizeByF[A](t: C3LBTree[A]): Int =
    fold[A,Int](t)((_:A) => 1)((lCount:Int, rCount:Int) => lCount+rCount+1)
  def tMaxIntByF(t: C3LBTree[Int]): Int =
    fold[Int,Int](t)((v:Int) => v)((lMax:Int, rMax:Int) => Math.max(lMax,rMax))
  def c3depthByF[A](t: C3LBTree[A]): Int = {
    { require (0 == depthOffset || 1 == depthOffset) }
    fold[A,Int](t)((_:A) => depthOffset)((lDepth:Int, rDepth:Int) => Math.max(lDepth+1,rDepth+1))
  }
  def c3mapByF[A,B](in: C3LBTree[A])(f: A => B): C3LBTree[B] =
    fold[A,C3LBTree[B]](in)((a:A) => C3LBLeaf(f(a)))((l:C3LBTree[B], r:C3LBTree[B]) => C3LBBranch(l,r))

  // not requested

  def leftLinearMap [A,B](t: C3LBTree[A])(f: A => B): C3Stack[B] = {

    import Chapter3L.{push,pop,isEmpty}

    @tailrec
    def pushTree(tree: C3LBTree[A], stack: C3Stack[C3LBTree[A]], acc: C3Stack[B]): C3Stack[B] = tree match {
      case C3LBLeaf(a) =>  /* tree -> acc */ popLeaf(stack, push(f(a), acc))
      case C3LBBranch(l,r) => /* tree -> stack */ pushTree(r,push(l,stack),acc)
    }

    @tailrec
    def popLeaf(stack: C3Stack[C3LBTree[A]], acc: C3Stack[B]): C3Stack[B] = stack match {
      case _ if isEmpty(stack) => acc
      case _ =>
        val (tr,trs) = pop(stack)
        tr match {
          case C3LBBranch(_,_) => /* stack -> tree */ pushTree(tr,trs,acc)
          case C3LBLeaf(a) =>  /* stack -> acc */ popLeaf(trs, push(f(a), acc))
        }
    }

    pushTree(t,C3Nil,C3Nil)
  }

  def deepLeft[A](leaves: C3Stack[C3LBLeaf[A]]): C3LBTree[A] = {

    import Chapter3L.{depth,pop,isEmpty}
    require(!isEmpty(leaves))
    val bCons: (C3LBTree[A], C3LBTree[A]) => C3LBTree[A] = (l:C3LBTree[A], r:C3LBTree[A]) => C3LBBranch(l,r)

    def go(depleting: C3Stack[C3LBTree[A]], acc: C3LBTree[A]): C3LBTree[A] = depleting match {
      case _ if isEmpty(depleting) => acc
      case _ =>
        val (l, bs) = pop(depleting)
        val (d,a) = bs match {
          case _ if isEmpty(bs) => (bs,bCons(acc,l))
          case _ =>
            val (r, remaining) = pop(bs)
            (remaining,bCons(acc,bCons(l,r)))
        }
        go(d,a)
    }

    val aux = depth(leaves)
    aux match {
      case 1 =>
        val (item, _ ) = pop(leaves)
        item
      case 2 =>
        val (l, nextStack) = pop(leaves)
        val (r, _ ) = pop(nextStack)
        bCons(l,r)
      case _ =>
        val (l, nextStack) = pop(leaves)
        val (r, s ) = pop(nextStack)
        go(s,bCons(l,r))
    }
  }

  def deepLeftMap[A,B](t: C3LBTree[A])(f: A => B): C3LBTree[B] = {
    val sol = leftLinearMap[A,C3LBLeaf[B]](t)((a:A) => C3LBLeaf(f(a)))
    deepLeft(sol)
  }
}
