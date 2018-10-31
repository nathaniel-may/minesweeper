package com.nathanielmay.minesweeper

case class H(value: Int)
case class V(value: Int)

// allows to force Dim case class creation through apply to make illegal states unrepresentable
sealed trait Dim {
  val h:    H
  val v:    V
  val area: Int

  def contains(s: Square): Boolean
  def contains(x: H):      Boolean
  def contains(x: V):      Boolean
}

// forces Dim case class creation through apply to make illegal states unrepresentable
object Dim {

  def apply(h: H, v: V): Option[Dim] = {
    val max = scala.math.sqrt(Int.MaxValue).toInt // area val cannot hit Int overflow
    if (h.value <= 0 || v.value <= 0 || h.value > max || v.value > max) None
    else Some(DimImpl(h, v))
  }

  private case class DimImpl(h: H, v: V) extends Dim {
    val area: Int = h.value * v.value

    def contains(s: Square): Boolean = contains(s.h) && contains(s.v)
    def contains(x: H):      Boolean = x.value >= 0 && x.value < h.value
    def contains(x: V):      Boolean = x.value >= 0 && x.value < v.value
  }

}
