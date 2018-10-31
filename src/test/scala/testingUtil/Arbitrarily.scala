package testingUtil


import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{choose, pick}

import scalaz._, Scalaz._

import com.nathanielmay.minesweeper._

import testingUtil.Util.{hAble, vAble, Run}

object Arbitrarily {
  import Generators._

  implicit val aDim: Arbitrary[Dim] = Arbitrary(dimGen)
  implicit val aRun: Arbitrary[Run] = Arbitrary(runGen)

}

private object Generators {
  //generating enormous minesweeper games takes forever to test
  //TODO testing with maxDim >= 100 => stackoverflow
  val maxDim = 50

  val dimGen: Gen[Dim] = for {
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

  val runGen: Gen[Run] = for {
    dim     <- dimGen
    bombs   <- choose(0, dim.area-1 )
    game    =  Game(dim, bombs).get
    squares <- genSquares(dim)
  } yield Run(game, squares)

  // so that Stream.sequence works
  implicit val genMonad: Monad[Gen] = GenMonad
  implicit val appGen: Applicative[Gen] = implicitly[Monad[Gen]]
  implicit object GenMonad extends Monad[Gen] {
    def point[A](a: => A): Gen[A] = Gen.const(a)
    def bind[A, B](ga: Gen[A])(f: A => Gen[B]): Gen[B] = ga flatMap f
  }

}
