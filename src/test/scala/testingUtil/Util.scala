package testingUtil

import com.nathanielmay.minesweeper.{H, MineSweeper, Game, EndGame, Square, V}

object Util {

  implicit def hAble(i: Int): H = H(i)
  implicit def vAble(i: Int): V = V(i)

  case class Run(game: Game, squares: Stream[Square]) {
    lazy val run: MineSweeper = game.revealAll(squares)
  }

  object TestableGame {
    def revealAll(game: MineSweeper, turns: Stream[Square]): MineSweeper = game match {
      case end:  EndGame => end
      case game: Game    => turns match {
        case sq #:: sqs   => revealAll(game.reveal(sq), sqs)
        case Stream.Empty => game
      }}
  }

  implicit class TestableGame(game: Game) {
    def revealAll(turns: List[Square]): MineSweeper =
      TestableGame.revealAll(game, turns.toStream)

    def revealAll(turns: Stream[Square]): MineSweeper =
      TestableGame.revealAll(game, turns)
  }

}
