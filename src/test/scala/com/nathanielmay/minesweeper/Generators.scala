package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.Gen.{choose, pick}
import org.scalacheck.Gen

// scala
import scala.language.implicitConversions

// project
import com.nathanielmay.minesweeper.Dim._
import testingUtil.Util.Run

//generating enormous minesweeper games takes forever to test
//values too high can cause a stackoverflow
object Generators {

  private implicit def hAble(i: Int): H = H(i)
  private implicit def vAble(i: Int): V = V(i)

  def dimGen(maxDim: Int): Gen[Dim] = for {
    h <- choose(1, maxDim)
    v <- if (h == 1) choose(2, maxDim) else choose(1, maxDim)
  } yield Dim(h, v).get

  def squareGen(dim: Dim): Gen[Square] = for {
    h <- choose(0, dim.h.value)
    v <- choose(0, dim.v.value)
  } yield Square(h, v)

  def badSquareGen(dim: Dim): Gen[Square] =
    Gen.oneOf(List(
      Square(dim.h+1, dim.v+1),
      Square(dim.h,   dim.v+1),
      Square(dim.h+1, dim.v),
      Square(-1,      -1),
      Square(0,       -1),
      Square(-1,      0) )
    )

  def badInputGen(maxDim: Int): Gen[(Dim, List[Square])] = for {
    dim   <- dimGen(maxDim)
    count <- Gen.choose(1, dim.area-1)
    bad   <- badSquareGen(dim)
    good  <- Gen.listOfN(count-1, squareGen(dim))
  } yield (dim, bad :: good)

  def gameGen(maxDim: Int): Gen[MineSweeper] = for {
    dim     <- dimGen(maxDim)
    bombs   <- choose(1, dim.area-1)
  } yield ActiveGame(dim, bombs).get

  def runGen(maxDim: Int): Gen[Run] =
    gameGen(maxDim).map(Run.apply)
}