package com.nathanielmay.minesweeper


case class H(value: Int)
case class V(value: Int)

case class Dim(h: H, v: V) {
  val area: Int = h.value * v.value

  def contains(s: Square): Boolean = contains(s.h) && contains(s.v)
  def contains(x: H):      Boolean = x.value >= 0 && x.value < h.value
  def contains(x: V):      Boolean = x.value >= 0 && x.value < v.value
  def isAtLeast(d: Dim):   Boolean = h.value >= d.h.value && v.value >= d.v.value
}

case class Square(h: H, v: V)

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

  //smart constructor called by other sugared apply methods
  private def apply(dim: Dim, graph: Map[Square, Tile]): Option[MineSweeper] = {
    if (!dim.isAtLeast(Dim(H(2), V(2)))) None
    else if (graph.values.count(_.value == Bomb) >= dim.area) None
    else if (graph.keys.exists(!dim.contains(_))) None
    else Some(new MineSweeper(dim, graph))
  }

  //standard game creation
  def apply(dim: Dim, bombs: Int): Option[MineSweeper] = {

    //quadratic function
    val randBombs = (0 until dim.area).toList.filterNot(
      List.tabulate(bombs)(x => (scala.math.random()*(dim.area-x)).toInt)
      .foldLeft((0 until dim.area).toList)((allSquares, bomb) =>
        allSquares.splitAt(bomb) match { case (pre, post) => pre ++ post.tail })
      .toSet)
      .map(rand => Square(H(rand / dim.h.value), V(rand % dim.h.value)))

    MineSweeper(dim, randBombs)
  }

  //custom game creation for testing
  //TODO move this into implicit test helper?
  def apply(dim: Dim, bombs: List[Square]): Option[MineSweeper] =
    MineSweeper(dim, buildGraph(dim, bombs))

  private def buildGraph(dim: Dim, bombs: List[Square]): Map[Square, Tile] = {
    def neighbors(square: Square): List[Square] =
      (for {
        hDiff <- -1 to 1
        vDiff <- -1 to 1
        if !(hDiff == 0 && vDiff == 0)
        neighbor = Square(H(square.h.value + hDiff), V(square.v.value + vDiff))
        if dim.contains(neighbor)
      } yield neighbor).toList

    val bombMap = bombs.map(_ -> Bomb).toMap

    (for{h <- 0 until dim.h.value; v <- 0 until dim.v.value} yield Square(H(h), V(v)))
      .map(square => square -> bombMap.getOrElse(square, NearBombs(-1)))
      .toMap
      .foldLeft(Map[Square, Tile]())((m, kv) => kv._2 match {
        case Bomb => m.updated(kv._1, Tile(Hidden, kv._2, neighbors(kv._1)))
        case NearBombs(_) => m.updated(kv._1, Tile(Hidden, NearBombs(neighbors(kv._1).count(bombMap.getOrElse(_, "") == Bomb)), neighbors(kv._1)))
      })
  }
}

//cannot make case class because of overloaded apply smart constructor
class MineSweeper private (dim: Dim, graph: Map[Square, Tile]) {

  //called when UI receives a click
  def reveal(square: Square): MineSweeper = {
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

    if (!dim.contains(square)) this
    else if (Hidden != graph(square).visibility) this
    else if (state.isDefined) this
    else MineSweeper(dim, floodReveal(square, graph)).get
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

  //for printing purposes
  override def toString: String = {
    (0 until dim.v.value).toList
      .map(v => (0 until dim.h.value).toList.map(h => Square(H(h), V(v))))
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
      .foldLeft(MineSweeper(Dim(4,4), List(Square(2,3), Square(0,2))).get)((game, square) =>
        takeTurn(game, square)
      ).state.getOrElse("Keep Playing"))
  }

  def takeTurn(game: MineSweeper, square: Square): MineSweeper = {
    println(game)
    println(game.state.getOrElse("Keep Playing"))
    println()
    game.reveal(square)
  }

  implicit def hAble(i: Int): H = H(i)
  implicit def vAble(i: Int): V = V(i)

}


