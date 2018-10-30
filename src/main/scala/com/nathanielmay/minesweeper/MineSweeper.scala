package com.nathanielmay.minesweeper

import Util.rand


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

// forces Dim csae class creation through apply to make illegal states unrepresentable
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
case object Win extends GameResult
case object Lose extends GameResult

//awkward construction until traits can take params: https://docs.scala-lang.org/sips/trait-parameters.html
sealed trait MineSweeper {
  val dim:     Dim
  val visible: Map[Square, MSValue]

  override def toString: String =
    (0 until dim.v.value).toList
      .map(v => (0 until dim.h.value).toList.map(h => Square(H(h), V(v))))
      .map(row => row.map(visible.getOrElse(_, " ").toString).mkString("|","|","|"))
      .mkString("\n")
}

object Game {
  //standard game creation
  def apply(dim: Dim, bombs: Int): Option[Game] = {
    //quadratic function TODO simplify
    def randBombs(b: Int): List[Square] = {
      import scalaz._, Scalaz._, scala.util.Random //TODO move?
      type Rng[A] = State[(Random, List[A]), A]
      def rng(n: Int): Rng[Int] =
        State[(Random, List[Int]), Int] {
          case (r, l) =>
            val next = r.nextInt(n)
            ((r, next :: l), next + l.count(_ < next)) }

      List.tabulate(b)(x => dim.area - x)
        .traverseS(rng)(new Random(System.currentTimeMillis()), List())._2
        .map(rand => Square(H(rand / dim.h.value), V(rand % dim.h.value)))
    }

    if (bombs >= dim.area || bombs < 0) None
    else Game(dim, randBombs(bombs))
  }

  //TODO make sure this is usable in tests while private
  def apply(dim: Dim, bombs: List[Square]): Option[Game] =
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
        neighbor = Square(H(square.h.value + hDiff), V(square.v.value + vDiff))
        if dim.contains(neighbor)
      } yield neighbor).toList

    //recursive function for when a zero is clicked
    //TODO can I avoid calling neighbors(toReveal) twice without a val?
    def floodReveal(toReveal: Square, midFlood: Map[Square, MSValue]): Map[Square, MSValue] = {
      NearBombs(neighbors(toReveal).count(bombs.contains)) match {
        case NearBombs(0) => neighbors(toReveal)
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

//object Play{
//
//  def main(args: Array[String]): Unit = {
//    play4x4()
//  }
//
//  def play4x4(): MineSweeper =
//    List(Square(0, 0),
//      Square(3, 3),
//      Square(1, 3),
//      Square(0, 3),
//      Square(0, 0))
//    .foldLeft[MineSweeper](Game(Dim(4,4), List(Square(2,3), Square(0,2))).get) {
//    (game, square) => {val next = takeTurn(game, square); println(s"$next\n"); next} }
//
//  def play2x2(): Unit = {
//    val g0 = Game(Dim(2,2), List(Square(0,0))).get
//    val g1 = takeTurn(g0, Square(1, 0))
//    val g2 = takeTurn(g1, Square(1, 1))
//    val g3 = takeTurn(g2, Square(0, 1))
//    println(List(g0, g1, g2, g3).mkString("", "\n\n", "\n"))
//  }
//
//  def takeTurn(game: MineSweeper, sq: Square): MineSweeper = game match {
//    case end:  EndGame => end
//    case game: Game    => game.reveal(sq)
//  }
//
//  implicit def hAble(i: Int): H = H(i)
//  implicit def vAble(i: Int): V = V(i)
//
//}


