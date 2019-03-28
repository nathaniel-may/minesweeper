package com.nathanielmay.minesweeper

import MineSweeper.{hToInt, vToInt}
import Dim.{H, V}
import shuffle.FunctionalShuffle, FunctionalShuffle.Rand


case class Square(h: H, v: V)

//sum type to disambiguate boolean flags
private sealed trait Visibility
private case object Revealed extends Visibility
private case object Hidden extends Visibility

//sum type for bombs and ints
sealed trait MSValue
case object Bomb extends MSValue{ override def toString = "B" }
case class NearBombs(n: Int) extends MSValue{
  override def toString: String = n.toString
}

//sum type for win/lose. Wrap in Option for games not won or lost yet
sealed trait GameResult
case object Win  extends GameResult
case object Lose extends GameResult

//awkward construction until traits can take params: https://docs.scala-lang.org/sips/trait-parameters.html
sealed trait MineSweeper {
  val dim:     Dim
  val visible: Map[Square, MSValue]

  override def toString: String =
    (0 until dim.v).toList
      .map(v => (0 until dim.h).toList.map(h => Square(H(h), V(v))))
      .map(row => row.map(visible.getOrElse(_, " ").toString).mkString("|","|","|"))
      .mkString("\n")
}

object MineSweeper{
  import FunctionalShuffle.shuffle
  import scala.util.Random
  import scalaz.State

  def indexToSquare(dim: Dim)(i: Int): Square =
    Square(H(i / dim.v), V(i % dim.v))

  def randBombs(dim: Dim, b: Int): Rand[List[Square]] = for {
    stream  <- shuffle[Boolean](Stream.fill(b)(true) #::: Stream.fill(dim.area - b)(false))
    bombs   =  stream.zipWithIndex.filter(_._1).map(_._2)
    squares =  bombs.map(indexToSquare(dim)).toList
  } yield squares

  implicit def hToInt(h: H): Int = h.value
  implicit def vToInt(v: V): Int = v.value
}

object Game {
  //standard game creation
  def apply(dim: Dim, bombs: Int): Option[Game] =
    if (bombs >= dim.area || bombs < 1) None
    else MineSweeper.randBombs(dim, bombs)
      .map { Game(dim, _) }
      .eval(new scala.util.Random(System.nanoTime()))

  private[minesweeper] def apply(dim: Dim, bombs: List[Square]): Option[Game] =
    if (bombs.forall(dim.contains)) Some(Game(dim, Map(), bombs))
    else None
}

case class Game private (dim: Dim, visible: Map[Square, MSValue], bombs: List[Square]) extends MineSweeper {
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
        case value: NearBombs => midFlood.updated(toReveal, value)
      }
    }

    if (bombs.contains(sq)) EndGame(dim, visible.updated(sq, Bomb), Lose)
    else Game(dim, floodReveal(sq, visible), bombs) match {
      case Game(d, v, bs) if v.size == d.area - bs.size => EndGame(d, v, Win)
      case g: Game => g
    }
  }
}

case class EndGame private (dim: Dim, visible: Map[Square, MSValue], state: GameResult) extends MineSweeper {
  override def toString: String = List(super.toString, state).mkString("\n")
}


