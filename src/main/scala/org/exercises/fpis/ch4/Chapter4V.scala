package org.exercises.fpis.ch4

import org.exercises.fpis.ch3.{C3List, C3Nil, Chapter3L}

object Chapter4V {

  def mean(xs: C3List[Double]): C4Option[Double] = xs match {
    case C3Nil => C4None
    case _ => C4Some(Chapter3L.foldLeft(xs,0.0)((last:Double, x: Double) => last+x) / Chapter3L.lenl(xs).toDouble)
  }
  def variance(xs: C3List[Double]): C4Option[Double] = mean(xs).map(mu =>
      Chapter3L.foldLeft(xs,0.0)((last:Double, x: Double) => last + Math.pow(x - mu,2.0)) / Chapter3L.lenl(xs).toDouble
    )
  def stdDev(xs: C3List[Double]): C4Option[Double] = variance(xs).map(Math.sqrt)
}
