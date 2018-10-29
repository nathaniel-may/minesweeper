package testingUtil

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{choose, oneOf}

import com.nathanielmay.minesweeper._

import testingUtil.Util.{hAble, vAble, Run}

object Arbitrarily {
  import Generators._

  implicit val aDim: Arbitrary[Dim] = Arbitrary(dimGen)
  implicit val aRun: Arbitrary[Run] = Arbitrary(runGen)

}

private object Generators {
  //generating enormous minesweeper games is expensive and shouldn't be constructed all at once
  val maxDim = 1000

  val dimGen: Gen[Dim] = for {
    h <- choose(1, maxDim)
    v <- if (h == 1) choose(2, maxDim) else choose(1, maxDim)
  } yield Dim(h, v).get

  def genAllSquares(dim: Dim): Gen[List[Square]] = {
    def go(remaining: List[Square], g: Gen[List[Square]]): Gen[List[Square]] = remaining match {
      case _ :: _ => for {
        sq <- oneOf(remaining)
      } yield go(remaining.filterNot(_ == sq), g.map(l => sq :: l))
      case Nil => g
    }

    val allSquares: List[Square] = (for {
      h <- 0 until dim.h.value
      v <- 0 until dim.v.value
    } yield Square(h, v)).toList

    go(allSquares, Gen.const(List()))
  }

  def genSquare(dim: Dim): Gen[Square] = for {
    h <- choose(0, dim.h.value)
    v <- choose(0, dim.v.value)
  } yield Square(h, v)

  val runGen: Gen[Run] = for {
    dim     <- dimGen
    bombs   <- choose(0, dim.area-1 )
    game    = Game(dim, bombs).get
    squares <- genAllSquares(dim)
  } yield Run(game, squares)

}
