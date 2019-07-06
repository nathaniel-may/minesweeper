package com.nathanielmay.minesweeper

// scalacheck
import org.scalacheck.{Properties, Prop}, Prop.forAll

// project
import testingUtil.Util.Run


object StringProperties extends Properties("Outputs of") {

  property("MSValues should be ints and `B`") = forAll {
    v: Option[Int] => v.fold[MSValue](Bomb)(NearBombs).toString == v.fold("B")(_.toString)
  }

  // TODO for 100% code coverage implement this prop
//  property("minesweeper games print correctly") = forAll {
//    run: Run => run.run match {
//      case g: EndGame => g.state match {
//        case Win  => ???
//        case Lose => ???
//      }
//      case g          => ???
//    }
//  }

}
