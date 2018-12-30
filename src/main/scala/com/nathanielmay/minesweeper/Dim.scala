package com.nathanielmay.minesweeper

case class H(value: Int)
case class V(value: Int)

object Dim {
  def apply(h: H, v: V): Option[Dim] = {
    val max = scala.math.sqrt(Int.MaxValue).toInt // area val cannot hit Int overflow
    if (h.value <= 0 || v.value <= 0 || h.value > max || v.value > max) None
    else Some(new Dim(h, v))
  }
}

// private constructor forces creation via smart constructor in companion object
case class Dim private (h: H, v: V) {
  val area: Int = h.value * v.value

  def contains(s: Square): Boolean = contains(s.h) && contains(s.v)
  def contains(x: H):      Boolean = x.value >= 0 && x.value < h.value
  def contains(x: V):      Boolean = x.value >= 0 && x.value < v.value
}
