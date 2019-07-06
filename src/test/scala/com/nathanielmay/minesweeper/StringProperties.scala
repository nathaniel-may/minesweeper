package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.{Arbitrary, Properties, Prop}, Prop.forAll

// project
import testingUtil.Util.Run
import Generators.runGen


object StringProperties extends Properties("Outputs of") {
  implicit val aRun: Arbitrary[Run] = Arbitrary(runGen(5))

  property("MSValues should be ints and `B`") = forAll {
    v: Option[Int] => v.fold[MSValue](Bomb)(NearBombs).toString == v.fold("B")(_.toString)
  }

  property("minesweeper game strings contain end state") = forAll {
    run: Run => run.run match {
      case g: FinalGame => g.state match {
        case Win  => g.toString.contains("Win")
        case Lose => g.toString.contains("Lose")
      }

      case g => !g.toString.contains("WIN") && !g.toString.contains("LOSE")
    }
  }

}
