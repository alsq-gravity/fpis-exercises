package org.exercises.fpis.ch3

sealed trait C3List[+A]
case object C3Nil extends C3List[Nothing]
case class C3Cons[+A](head: A, tail: C3List[A]) extends C3List[A]

sealed trait C3Tree[+A]
case class C3Leaf[+A](v: A) extends C3Tree[A]
case class C3Branch[+A](l: C3Tree[A], r: C3Tree[A]) extends C3Tree[A]
case object C3Stump extends C3Tree[Nothing]