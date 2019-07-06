package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.Gen.{choose, pick}
import org.scalacheck.Gen

// project
import com.nathanielmay.minesweeper.Dim._
import testingUtil.Util.{Run, hAble, vAble}

//generating enormous minesweeper games takes forever to test
//values too high can cause a stackoverflow
object Generators {

  def dimGen(maxDim: Int): Gen[Dim] = for {
    h <- choose(1, maxDim)
    v <- if (h == 1) choose(2, maxDim) else choose(1, maxDim)
  } yield Dim(h, v).get

  def squareStreamGen(dim: Dim): Gen[Stream[Square]] =
    pick(dim.area, allSquares(dim)).map(_.toStream)

  def allSquares(dim: Dim): List[Square] = (for {
    h <- 0 until dim.h.value
    v <- 0 until dim.v.value
  } yield Square(H(h), V(v))).toList

  def squareGen(dim: Dim): Gen[Square] = for {
    h <- choose(0, dim.h.value)
    v <- choose(0, dim.v.value)
  } yield Square(h, v)

  def badSquareGen(dim: Dim): Gen[Square] = {
    val badSquares = List(
      Square(dim.h+1, dim.v+1),
      Square(dim.h,   dim.v+1),
      Square(dim.h+1, dim.v),
      Square(-1,      -1),
      Square(0,       -1),
      Square(-1,      0) )

    Gen.pick(1, badSquares).flatMap(_.head)
  }

  def badInputGen(maxDim: Int): Gen[(Dim, List[Square])] = for {
    dim   <- dimGen(maxDim)
    count <- Gen.choose(1, dim.area-1)
    bad   <- badSquareGen(dim)
    good  <- Gen.listOfN(count-1, squareGen(dim))
  } yield (dim, bad :: good)

  def runGen(maxDim: Int): Gen[Run] = for {
    dim     <- dimGen(maxDim)
    bombs   <- choose(1, dim.area-1)
    squares <- squareStreamGen(dim)
    run     <- ActiveGame(dim, bombs) match {
                 case Some(g: ActiveGame) => Gen.const(Run(g, squares))
                 case _                   => runGen(maxDim)
               }
  } yield run
}