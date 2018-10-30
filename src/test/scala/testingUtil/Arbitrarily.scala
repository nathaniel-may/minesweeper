package testingUtil


import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{choose, oneOf}

import scalaz._, Scalaz._

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

  def genSquares(dim: Dim): Gen[Stream[Square]] =
    unfold[Dim, Gen[Square]](dim) { d => (genSquare(d), d).some }
      .sequence

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
