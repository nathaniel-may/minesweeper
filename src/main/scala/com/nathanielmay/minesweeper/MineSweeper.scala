package com.nathanielmay.minesweeper

import Dim.{H, V, hToInt, vToInt}
import shuffle.FunctionalShuffle, FunctionalShuffle.{shuffle, Rand}


case class Square(h: H, v: V)

//sum type to disambiguate boolean flags
private sealed trait Visibility
private case object Revealed extends Visibility
private case object Hidden extends Visibility

//sum type for bombs and ints
sealed trait MSValue
case object Bomb extends MSValue { override def toString = "B" }
case class NearBombs(n: Int) extends MSValue{
  override def toString: String = n.toString
}

//sum type for win/lose. Wrap in Option for games not won or lost yet
sealed trait GameResult
case object Win  extends GameResult
case object Lose extends GameResult

sealed trait MineSweeper {
  val dim:     Dim
  val visible: Map[Square, MSValue]

  override def toString: String =
    (0 until dim.v).toList
      .map { v => (0 until dim.h).toList.map { h => Square(H(h), V(v)) } }
      .map { row => row.map(visible.getOrElse(_, " ").toString).mkString("|","|","|") }
      .mkString("\n")
}

object MineSweeper {
  def indexToSquare(dim: Dim)(i: Int): Square =
    Square(H(i / dim.v), V(i % dim.v))

  def randBombs(dim: Dim, b: Int): Rand[List[Square]] =
    shuffle(Stream.fill(b)(true) #::: Stream.fill(dim.area - b)(false))
      .map {
        _.zipWithIndex
        .filter(_._1)
        .map { case (_, idx) => indexToSquare(dim)(idx) }
        .toList
      }
}

object ActiveGame {
  def apply(dim: Dim, bombs: Int): Option[MineSweeper] =
    if (bombs >= dim.area || bombs < 1)
      None
    else MineSweeper
      .randBombs(dim, bombs)
      .map { bombs =>
        if (bombs.forall(dim.contains))
          ActiveGame(dim, Map(), bombs)
        else
          None
      }
      .eval(new scala.util.Random(System.nanoTime()))

  private[minesweeper] def apply(dim: Dim, visible: Map[Square, MSValue], bombs: List[Square]): Option[MineSweeper] =
    if (bombs.map(dim.contains).exists(!_))
      None
    else if (visible.values.toStream.contains(Bomb))
      Some(FinalGame(dim, visible, Lose))
    else if (visible.size == dim.area - bombs.size)
      Some(FinalGame(dim, visible, Win))
    else
      Some(new ActiveGame(dim, visible, bombs))
}

case class ActiveGame private (dim: Dim, visible: Map[Square, MSValue], bombs: List[Square]) extends MineSweeper {
  //called when UI receives a click
  def reveal(sq: Square): MineSweeper = {
    def neighbors(square: Square): List[Square] =
      (for {
        hDiff <- -1 to 1
        vDiff <- -1 to 1
        if !(hDiff == 0 && vDiff == 0)
        neighbor = Square(H(square.h + hDiff), V(square.v + vDiff))
        if dim.contains(neighbor)
      } yield neighbor).toList

    //recurses when a zero is clicked
    def floodReveal(toReveal: Square, midFlood: Map[Square, MSValue]): Map[Square, MSValue] = {
      val ns = neighbors(toReveal)
      NearBombs(ns.count(bombs.contains)) match {
        case NearBombs(0) => ns
          .filterNot(midFlood.contains)
          .foldLeft(midFlood.updated(toReveal, NearBombs(0))) {
            (m, sq) => floodReveal(sq, m) }
        case value => midFlood.updated(toReveal, value)
      }
    }

    if (bombs.contains(sq))
      FinalGame(dim, visible.updated(sq, Bomb), Lose)
    else
      ActiveGame(dim, floodReveal(sq, visible), bombs).get
  }
}

case class FinalGame private (dim: Dim, visible: Map[Square, MSValue], state: GameResult) extends MineSweeper {
  override def toString: String = List(super.toString, state).mkString("\n")
}


