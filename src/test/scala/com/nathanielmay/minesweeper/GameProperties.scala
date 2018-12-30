package com.nathanielmay.minesweeper

//scalacheck
import org.scalacheck.{Properties, Prop, Gen}, Prop.{BooleanOperators, forAll}, Gen.choose

//testing
import testingUtil.Arbitrarily.{aDim, getDimGen, aRun}
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

  property("randomly played games always win or lose") = forAll {
    run: Run => run.run match {
      case _: EndGame => true
      case _          => false
    }
  }

  property("randBombs always generates within the dimension") = forAll {
    (seed: Long, b: Int, d: Dim) =>
      (b >= 0 && b < d.area) ==> (for {
      bombs <- randBombs(d, b)
    } yield bombs.forall(d.contains)).eval(new java.util.Random(seed))
  }

  //TODO not a pure property
  property("randBombs generates bombs in the full range of tiles") = forAll(getDimGen(7)) {
    (dim: Dim) => {
      val bombs: Int = scala.math.ceil(dim.area / 5.0).toInt

      Stream.fill(dim.area * dim.area / bombs)(randBombs(dim, bombs))
        .foldLeft[Set[Square]](Set()) { (l, rand) =>
          rand.eval(new scala.util.Random(System.nanoTime())).toSet ++ l }
        .size == dim.area
    }
  }

}
