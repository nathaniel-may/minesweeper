package testingUtil

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Gen.choose

import com.nathanielmay.minesweeper.{Dim, H, V}
import testingUtil.Util.{hAble, vAble}

object Arbitrarily {
  import Generators._

  implicit val aDim:           Arbitrary[Dim] = Arbitrary(dimGen)

}

private object Generators {

  //generating enormous minesweeper games is expensive and shouldn't be constructed all at once
  val maxDim = 1000

  val dimGen: Gen[Dim] = for {
    h <- choose(1, maxDim)
    v <- if (h == 1) choose(2, maxDim) else choose(1, maxDim)
  } yield Dim(h, v).get

}
