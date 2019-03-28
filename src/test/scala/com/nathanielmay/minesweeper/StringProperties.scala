package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.{Properties, Prop, Gen}, Prop.{BooleanOperators, forAll}, Gen.choose

// scala
import scalaz.Scalaz.unfold

// project
import testingUtil.Arbitrarily.{aDim, getDimGen, aRun}
import testingUtil.Util.Run
import MineSweeper.randBombs

object StringProperties extends Properties("Outputs of") {

  property("MSValues should be ints and `B`") = forAll {
    v: Option[Int] => v.fold[MSValue](Bomb)(NearBombs).toString == v.fold("B")(_.toString)
  }

  // TODO game with apply(DIM, BOMBS:List[Square]) prints correctly.

}
