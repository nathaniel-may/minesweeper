package com.nathanielmay.minesweeper

//scalacheck
import org.scalacheck.{Properties, Prop}, Prop.{BooleanOperators, forAll, exists}

//testing
import testingUtil.Arbitrarily.{aDim, aRun}
import testingUtil.Util.{Run, TestableGame}

//project

object GameProperties extends Properties("MineSweeper game"){

  //TODO put in own object
  property("dim must be at least 1x1 and less than sqrt(Int.MaxValue)") = forAll {
    (dh: Int, dv: Int) => (dh, dv) match {
      case (h, v) =>
        if (List(h, v, h*v).exists(_ < 1) || List(h, v).exists(_ > scala.math.sqrt(Int.MaxValue)))
          Dim(H(h), V(v)).isEmpty
        else Dim(H(h), V(v)).isDefined
    }
  }

  property("can't have negative bombs") = forAll {
    (d: Dim, i: Int) =>
      (i < 0) ==> Game(d, i).isEmpty
  }

  property("can't have more bombs than tiles") = forAll {
    (d: Dim, i: Int) =>
      (i >= d.area) ==> Game(d, i).isEmpty
  }

//  property("can win a game") = exists {
//    run: Run => run.run match {
//        case EndGame(_, _, result) => result == Win
//        case _                     => false
//      }
//  }

  //TODO put it util testing object
  property("randBombs should always generate within the dimension") = forAll {
    (seed: Long, b: Int, d: Dim) =>
      (b >= 0 && b < d.area) ==> Util.randBombs(seed)(d, b).forall(d.contains)
  }

}
