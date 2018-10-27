package com.nathanielmay.minesweeper

case class Square(h: Int, v: Int)

//sum type to disambiguate boolean flags
private sealed trait Visibility
private case object Revealed extends Visibility
private case object Hidden extends Visibility

private case class Tile(visibility: Visibility, value: MSValue, neighbors: List[Square]){
  def reveal: Tile = copy(visibility = Revealed)
}

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


object MineSweeper{

  //to be called by companion object only
  private def apply(hDim: Int, vDim: Int, graph: Map[Square, Tile]): MineSweeper = new MineSweeper(hDim, vDim, graph)

  //standard game creation
  def apply(hDim: Int, vDim: Int, bombs: Int): MineSweeper = {
    require(hDim > 1 && vDim > 1, "size of map must be at least 2x2")
    require(bombs < hDim*vDim, "map must have at least one square without a bomb")

    //quadratic function
    val randBombs = (0 until hDim*vDim).toList.filterNot(
      List.tabulate(bombs)(x => (scala.math.random()*(hDim*vDim-x)).toInt)
      .foldLeft((0 until hDim*vDim).toList)((allSquares, bomb) =>
        allSquares.splitAt(bomb) match { case (pre, post) => pre ++ post.tail })
      .toSet)
      .map(rand => Square(rand/hDim.toInt, rand%hDim))

    MineSweeper(hDim, vDim, randBombs)
  }

  //custom game creation for testing
  def apply(hDim: Int, vDim: Int, bombs: List[Square]): MineSweeper = {
    require(hDim > 1 && vDim > 1, "size of map must be at least 2x2")
    require(bombs.size < hDim*vDim, "map must have at least one square without a bomb")
    require(!bombs.exists(s => s.h >= hDim || s.h < 0 || s.v >= vDim || s.v < 0), "all bombs must be within the bounds of the map")

    MineSweeper(hDim, vDim, buildGraph(hDim, vDim, bombs))
  }

  private def buildGraph(hDim: Int, vDim: Int, bombs: List[Square]): Map[Square, Tile] = {
    def neighbors(square: Square): List[Square] =
      (for {
        hDiff <- -1 to 1
        vDiff <- -1 to 1
        if !(hDiff == 0 && vDiff == 0)
        neighbor = Square(square.h + hDiff, square.v + vDiff)
        if withinBounds(neighbor, hDim, vDim)
      } yield neighbor).toList

    val bombMap = bombs.map(_ -> Bomb).toMap

    (for{h <- 0 until hDim; v <- 0 until vDim} yield Square(h, v))
      .map(square => square -> bombMap.getOrElse(square, NearBombs(-1)))
      .toMap
      .foldLeft(Map[Square, Tile]())((m, kv) => kv._2 match {
        case Bomb => m.updated(kv._1, Tile(Hidden, kv._2, neighbors(kv._1)))
        case NearBombs(_) => m.updated(kv._1, Tile(Hidden, NearBombs(neighbors(kv._1).count(bombMap.getOrElse(_, "") == Bomb)), neighbors(kv._1)))
      })
  }

  private def withinBounds(square: Square, hDim: Int, vDim: Int): Boolean =
    square.h < hDim && square.h >= 0 &&
    square.v < vDim && square.v >= 0
}

class MineSweeper private (hDim: Int, vDim: Int, graph: Map[Square, Tile]) {

  //called when UI receives a click
  def reveal(square: Square): MineSweeper = {
    require(withinBounds(square), "choose a square within the bounds of the board")
    require(Hidden == graph(square).visibility, "this tile has already been revealed")
    require(state.isEmpty, "cannot continue a completed game")

    //recursive function for when a zero is clicked
    def floodReveal(toReveal: Square, midFloodGraph: Map[Square, Tile]): Map[Square, Tile] = {
      graph(toReveal).value match {
        case Bomb => midFloodGraph //floods should stop at numbers and not reach bombs anyway
        case NearBombs(n) if n >  0 => midFloodGraph.updated(toReveal, graph(toReveal).reveal)
        case NearBombs(n) if n <= 0 => if(Revealed == midFloodGraph(toReveal).visibility) midFloodGraph else
          graph(toReveal).neighbors.foldLeft(midFloodGraph.updated(toReveal, graph(toReveal).reveal))((m, neighbor) =>
            floodReveal(neighbor, m))
      }
    }

    MineSweeper(hDim, vDim, floodReveal(square, graph))
  }

  //exposing only public types to UI
  lazy val revealedTiles: Map[Square, MSValue] =
    graph.filter(_._2.visibility == Revealed).mapValues(_.value)

  lazy val state: Option[GameResult] = {

    def stateCheck(tiles: List[Tile], bombs: Int, hidden: Int): Option[GameResult] =
      tiles match {
        case Nil if hidden == bombs => Some(Win)
        case Nil if hidden >  bombs => None
        case Nil if hidden <  bombs => Some(Lose)
        case h :: _ if Revealed == h.visibility && h.value == Bomb => Some(Lose)
        case h :: t => stateCheck(t,
                                  if (Bomb   == h.value)      bombs  + 1 else bombs,
                                  if (Hidden == h.visibility) hidden + 1 else hidden)
      }

    stateCheck(graph.values.toList, 0, 0)
  }

  private def withinBounds(square: Square): Boolean =
    MineSweeper.withinBounds(square, hDim, vDim)

  //for printing purposes
  override def toString: String = {
    (0 until vDim).toList
      .map(v => (0 until hDim).toList.map(h => Square(h, v)))
      .map(row => row.map(revealedTiles.getOrElse(_, " ").toString).mkString("|","|","|"))
      .mkString("\n")
  }

}

object Play{

  def main(args: Array[String]): Unit = {
    println(List(Square(0, 0),
         Square(3, 3),
         Square(1, 3),
         Square(0, 3))
      .foldLeft(MineSweeper(4, 4, List(Square(2,3), Square(0,2))))((game, square) =>
        takeTurn(game, square)
      ).state.getOrElse("Keep Playing"))
  }

  def takeTurn(game: MineSweeper, square: Square): MineSweeper = {
    println(game)
    println(game.state.getOrElse("Keep Playing"))
    println()
    game.reveal(square)
  }

}


