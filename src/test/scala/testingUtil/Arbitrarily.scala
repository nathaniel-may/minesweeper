package testingUtil

// scalacheck
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{choose, pick}

// project
import com.nathanielmay.minesweeper._, Dim.{H, V}
import testingUtil.Util.{hAble, vAble, Run}

object Arbitrarily {
  import Generators._

  implicit val aDim: Arbitrary[Dim] = Arbitrary(dimGen(20))
  implicit val aRun: Arbitrary[Run] = Arbitrary(runGen(20))

  val getDimGen: Int => Gen[Dim] = dimGen

}

private object Generators {
  //generating enormous minesweeper games takes forever to test
  //values too high can cause stackoverflow

  def dimGen(maxDim: Int): Gen[Dim] = for {
    h <- choose(1, maxDim)
    v <- if (h == 1) choose(2, maxDim) else choose(1, maxDim)
  } yield Dim(h, v).get

  def genSquares(dim: Dim): Gen[Stream[Square]] =
    pick(dim.area, allSquares(dim)).map(_.toStream)

  def allSquares(dim: Dim): List[Square] = (for {
    h <- 0 until dim.h.value
    v <- 0 until dim.v.value
  } yield Square(H(h), V(v))).toList

  def genSquare(dim: Dim): Gen[Square] = for {
    h <- choose(0, dim.h.value)
    v <- choose(0, dim.v.value)
  } yield Square(h, v)

  def runGen(maxDim: Int): Gen[Run] = for {
    dim     <- dimGen(maxDim)
    bombs   <- choose(0, dim.area-1 )
    game    =  Game(dim, bombs).get
    squares <- genSquares(dim)
  } yield Run(game, squares)

}
