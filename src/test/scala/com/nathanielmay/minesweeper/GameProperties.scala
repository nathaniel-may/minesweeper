package com.nathanielmay.minesweeper

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}

//testing
import testingUtil.Arbitrarily.aDim

//project

object GameProperties extends Properties("Board"){

  //TODO put in own object
  property("dim must be at least 1x1 and less than sqrt(Int.MaxValue)") = forAll {
    (dh: Int, dv: Int) => (dh, dv) match {
      case (h, v) =>
        if (List(h, v, h*v).exists(_ < 1) || List(h, v).exists(_ > scala.math.sqrt(Int.MaxValue)))
          Dim(H(h), V(v)).isEmpty
        else Dim(H(h), V(v)).isDefined
    }
  }

//  property("can't have more bombs than tiles") = forAll {
//    (d: Dim, i: Int) =>
//      (i >= d.area) ==> Game(d, i).isEmpty
//  }

}
