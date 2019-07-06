package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.{BooleanOperators, forAll}
import Gen.choose

// scala
import scalaz.Scalaz.unfold

// project
import Generators._
import testingUtil.Util.Run
import MineSweeper.randBombs

object MineSweeperProperties extends Properties("A MineSweeper game") {
  val maxDim = 20
  implicit val aDim: Arbitrary[Dim] = Arbitrary(dimGen(maxDim))
  implicit val aRun: Arbitrary[Run] = Arbitrary(runGen(maxDim))
  implicit val aSquare: Arbitrary[Square] = Arbitrary(squareGen(maxDim))

  property("must have more than one bomb") = forAll {
    (d: Dim, i: Int) =>
      (i < 1) ==> ActiveGame(d, i).isEmpty
  }

  property("can't have more bombs than tiles") = forAll {
    (d: Dim, i: Int) =>
      (i >= d.area) ==> ActiveGame(d, i).isEmpty
  }

  property("randomly played games win or lose by at most revealing each square once") = forAll {
    run: Run => run.run match {
      case _: FinalGame => true
      case _            => false
    }
  }

  property("randBombs always generates within the dimension") = forAll {
    (seed: Long, b: Int, d: Dim) =>
      (b >= 0 && b < d.area) ==> (for {
      bombs <- randBombs(d, b)
    } yield bombs.forall(d.contains)).eval(new java.util.Random(seed))
  }

  property("with bombs specified outside the dimension are rejected") =
    forAll(badInputGen(maxDim)) {
      bad: (Dim, List[Square]) =>
        val (dim, bombs) = bad
        ActiveGame(dim, Map(), bombs).isEmpty
    }

  property("randBombs generates bombs in the full range of tiles") =
    forAll(choose(Long.MinValue, Long.MaxValue), dimGen(7)) {
      (seed: Long, dim: Dim) => {
        def noLessThan(d: Double, min: Int): Int =
          if (d.toInt < min) min else d.toInt

        val infiniteSamples: Stream[Set[Square]] =
          unfold(new scala.util.Random(seed)) { r =>
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

  property("square.fromIndex works") = forAll {
    (dim: Dim, idx: Int) =>
      Square.fromIndex(dim)(idx) match {
        case None     => idx < 0 || idx >= dim.area
        case Some(sq) => sq.h.value * dim.v + sq.v.value == idx
      }
  }

  property("ActiveGame public constructor works") = forAll {
    (dim: Dim, bombs: Int) =>
      ActiveGame(dim, bombs) match {
        case None    => bombs <= 0 || bombs >= dim.area
        case Some(_) => bombs >  0 && bombs <  dim.area
      }
  }

  property("ActiveGame private constructor works") = forAll {
    (dim: Dim, bombs: List[Square]) =>
      val visible: Map[Square, MSValue] = bombs.headOption match {
        case None     => Map()
        case Some(sq) => Map(sq -> Bomb)
      }

      // bombs in visible aren't allowed and games with no bombs aren't allowed
      ActiveGame(dim, visible, bombs).isEmpty
  }

}
