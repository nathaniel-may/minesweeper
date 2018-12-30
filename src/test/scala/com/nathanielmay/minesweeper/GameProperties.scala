package com.nathanielmay.minesweeper

//scalacheck
import org.scalacheck.{Properties, Prop, Gen}, Prop.{BooleanOperators, forAll}, Gen.choose

//testing
import testingUtil.Arbitrarily.{aDim, aRun}
import testingUtil.Util.{Run, TestableGame}

//project
import MineSweeper.randBombs

object GameProperties extends Properties("MineSweeper game"){

  property("can't have negative bombs") = forAll {
    (d: Dim, i: Int) =>
      (i < 0) ==> Game(d, i).isEmpty
  }

  property("can't have more bombs than tiles") = forAll {
    (d: Dim, i: Int) =>
      (i >= d.area) ==> Game(d, i).isEmpty
  }

//  property("randomly played games always win or lose") = forAll {
//    run: Run => run.run match { case _: EndGame => true }
//  }

//  //TODO put it util testing object
//  property("randBombs always generates within the dimension") = forAll {
//    (seed: Long, b: Int, d: Dim) =>
//      (b >= 0 && b < d.area) ==> (for {
//      bombs <- randBombs(d, b)
//    } yield bombs.forall(d.contains)).eval(new java.util.Random(seed))
//  }

  //TODO property for randBombs can generate bombs in the full range of tiles

}
