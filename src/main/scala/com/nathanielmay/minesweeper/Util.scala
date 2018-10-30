package com.nathanielmay.minesweeper

import scalaz._, Scalaz._
import scala.util.Random

object Util {

  def specialRng(n: Int): State[(Random, List[Int]), Int] =
    State[(Random, List[Int]), Int] {
      case (r, l) =>
        val nextRaw = r.nextInt(n)
        val next    = nextRaw + l.count(_ <= nextRaw)
        ((r, next :: l), next ) }

  def randBombs(seed: Long)(dim: Dim, b: Int): List[Square] =
    List.tabulate(b)(x => dim.area - x)
      .traverseS(specialRng)(new Random(seed), List())._2
      .map(rand => Square(H(rand / dim.v.value), V(rand % dim.v.value)))

}