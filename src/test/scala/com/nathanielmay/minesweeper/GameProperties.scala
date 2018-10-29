package com.nathanielmay.minesweeper

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}

//testing
import testingUtil.Arbitrarily.anyDim

//project

object GameProperties extends Properties("Board"){

  property("cannot create a game smaller than 2x2") = forAll {
    d: Dim =>
      !d.isAtLeast(Dim(H(2), V(2))) ==> Game(d, 1).isEmpty
  }

}
