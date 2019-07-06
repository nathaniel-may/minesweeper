package testingUtil

import com.nathanielmay.minesweeper.{ActiveGame, FinalGame, MineSweeper, Square}
import scala.annotation.tailrec

object Util {

  final case class Run(game: MineSweeper) {
    private val moves: Stream[Square] =
      (0 until game.dim.area)
        .toStream
        .flatMap(Square.fromIndex(game.dim))

    def run: MineSweeper = {
      @tailrec
      def go(g: MineSweeper, m: Stream[Square]): MineSweeper =
        m match {
          case Stream.Empty => game
          case sq #:: sqs   => g match {
            case g: ActiveGame => go(g.reveal(sq), sqs)
            case g: FinalGame  => g
          }
        }

      go(game, moves)
    }
  }

}
