package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.{Properties, Prop, Gen}, Prop.{BooleanOperators, forAll}, Gen.choose

// scala
import scalaz.Scalaz.unfold

// project
import testingUtil.Arbitrarily.{aDim, getDimGen, aRun}
import testingUtil.Util.Run
import MineSweeper.randBombs

object GameProperties extends Properties("MineSweeper game") {

  property("can't have negative bombs") = forAll {
    (d: Dim, i: Int) =>
      (i < 1) ==> Game(d, i).isEmpty
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

  property("randBombs generates bombs in the full range of tiles") = forAll(choose(Long.MinValue, Long.MaxValue), getDimGen(7)) {
    (seed: Long, dim: Dim) => {
      def noLessThan(d: Double, min: Int): Int = if (d.toInt < min) min else d.toInt
      val infiniteSamples: Stream[Set[Square]] = unfold(new scala.util.Random(seed)) { r =>
        Some((randBombs(dim, noLessThan(dim.area / 2.0, 1)).eval(r).toSet, r)) }

      def go(set: Set[Square], samples: Stream[Set[Square]], attempts: Int): Boolean =
        if (set.size == dim.area) true
        else if (attempts <= 0) false
        else samples match {
          case sample #:: moreSamples => go(set ++ sample, moreSamples, attempts-1)
          case _                      => false
        }

      go(Set(), infiniteSamples, 100)
    }
  }

}
