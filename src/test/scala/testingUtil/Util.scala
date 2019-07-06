package testingUtil

import com.nathanielmay.minesweeper.{Square, MineSweeper, ActiveGame, FinalGame}

object Util {

  // TODO make def runToEnd: Final and def run: Active
  case class Run(game: ActiveGame, squares: Stream[Square]) {
    val run: MineSweeper = game.revealAll(squares)
    override def toString: String = List("", run, squares).mkString("\n")
  }

  // TODO move into Run
  object TestableGame {
    // explicitly tail recursive here rather than defining in class
    def revealAll(game: MineSweeper, turns: Stream[Square]): MineSweeper = game match {
      case end:  FinalGame => end
      case game: ActiveGame    => turns match {
        case sq #:: sqs   => revealAll(game.reveal(sq), sqs)
        case Stream.Empty => game
      }}
  }

  // TODO unnecessary implicit
  implicit class TestableGame(game: ActiveGame) {
    def revealAll(turns: List[Square]): MineSweeper =
      TestableGame.revealAll(game, turns.toStream)

    def revealAll(turns: Stream[Square]): MineSweeper =
      TestableGame.revealAll(game, turns)
  }

}
