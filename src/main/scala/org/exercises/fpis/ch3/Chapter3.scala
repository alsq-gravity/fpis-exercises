package org.exercises.fpis.ch3

sealed trait C3List[+A]
case object C3Nil extends C3List[Nothing]
case class C3Cons[+A](head: A, tail: C3List[A]) extends C3List[A]

sealed trait C3LBTree[+A] // Leaf-Branch
case class C3LBLeaf[+A](v: A) extends C3LBTree[A]
case class C3LBBranch[+A](l: C3LBTree[A], r: C3LBTree[A]) extends C3LBTree[A]

sealed trait C3SNTree[+A] // Stump-Node
case object C3SNStump extends C3SNTree[Nothing]
case class C3SNNode[+A](v:A, l: C3SNTree[A], r: C3SNTree[A]) extends C3SNTree[A]

sealed trait C3XTree[+A] // Extended
case class C3XLeaf[+A](v: A) extends C3XTree[A]
case class C3XBranch[+A](l: C3XTree[A], r: C3XTree[A]) extends C3XTree[A]
case object C3XStump extends C3XTree[Nothing]

