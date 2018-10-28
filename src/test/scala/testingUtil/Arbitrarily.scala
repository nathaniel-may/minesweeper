package testingUtil

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Gen.choose

import com.nathanielmay.minesweeper.{Dim, H, V}
import testingUtil.Util.{hAble, vAble}

object Arbitrarily {
  import Generators._

  implicit val anyDim: Arbitrary[Dim] = Arbitrary(dimGen)
  implicit val aDim:   Arbitrary[Dim] = Arbitrary(validDimGen)

}

private object Generators {

  //generating enormous minesweeper games is expensive and shouldn't be done strictly
  val maxDim = 1000

  val dimGen: Gen[Dim] =
    dimAtLeast(Dim(Int.MinValue, maxDim))

  val validDimGen: Gen[Dim] =
    dimAtLeast(Dim(2, 2))

  def dimAtLeast(d: Dim): Gen[Dim] = for {
    h <- choose(d.h.value, maxDim)
    v <- choose(d.v.value, maxDim)
  } yield Dim(h, v)

}
