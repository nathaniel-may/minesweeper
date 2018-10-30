package com.nathanielmay.minesweeper

import scalaz._, Scalaz._
import scala.util.Random

object Util {

  def indexToSquare(dim: Dim)(i: Int): Square =
    Square(H(i / dim.v.value), V(i % dim.v.value))

  def specialRng(n: Int): State[(Random, List[Int]), Int] =
    State[(Random, List[Int]), Int] {
      case (r, l) =>
        val nextRaw = r.nextInt(n)
        val next    = nextRaw + l.count(_ <= nextRaw)
        ((r, next :: l), next ) }

  def randBombs(seed: Long)(dim: Dim, b: Int): List[Square] =
    List.tabulate(b)(x => dim.area - x)
      .traverseS(specialRng)(new Random(seed), List())._2
      .map(indexToSquare(dim))

}