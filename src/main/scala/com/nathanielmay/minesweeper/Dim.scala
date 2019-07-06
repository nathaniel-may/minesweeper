package com.nathanielmay.minesweeper

import Dim.{H, V}
import scala.language.implicitConversions

object Dim {
  final case class H(value: Int)
  final case class V(value: Int)

  private[minesweeper] implicit def hToInt(h: H): Int = h.value
  private[minesweeper] implicit def vToInt(v: V): Int = v.value

  def apply(h: H, v: V): Option[Dim] = {
    val max = scala.math.sqrt(Int.MaxValue).toInt // area val cannot hit Int overflow
    if (h.value <= 0 || v.value <= 0 || h.value > max || v.value > max) None
    else Some(new Dim(h, v))
  }
}

final case class Dim private (h: H, v: V) {
  val area: Int = h.value * v.value

  def contains(s: Square): Boolean = contains(s.h) && contains(s.v)
  def contains(x: H):      Boolean = x.value >= 0 && x.value < h.value
  def contains(x: V):      Boolean = x.value >= 0 && x.value < v.value
}