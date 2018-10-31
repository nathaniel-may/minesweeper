package com.nathanielmay.minesweeper

import org.scalacheck.{Properties, Prop, Gen}, Prop.{BooleanOperators, forAll}

object DimProperties extends Properties("MineSweeper game") {

  property("dim must be at least 1x1 and less than sqrt(Int.MaxValue)") = forAll {
    (dh: Int, dv: Int) => (dh, dv) match {
      case (h, v) =>
        if (List(h, v, h*v).exists(_ < 1) || List(h, v).exists(_ > scala.math.sqrt(Int.MaxValue)))
          Dim(H(h), V(v)).isEmpty
        else Dim(H(h), V(v)).isDefined
    }
  }

  property("dims less than 1x1 and more than sqrt(Int.MaxValue) are empty") = forAll {
    (h: Int, v: Int) =>
      (List(h, v, h*v).exists(_ < 1) || List(h, v).exists(_ > scala.math.sqrt(Int.MaxValue))) ==>
          Dim(H(h), V(v)).isEmpty
  }

  val goodDim: Gen[Int] = Gen.choose(1, scala.math.sqrt(Int.MaxValue).toInt)
  property("dims at least 1x1 and less than sqrt(Int.MaxValue) are defined") = forAll(goodDim, goodDim) {
    (h: Int, v: Int) =>
      !(List(h, v, h*v).exists(_ < 1) || List(h, v).exists(_ > scala.math.sqrt(Int.MaxValue))) ==>
        Dim(H(h), V(v)).isDefined
  }

}
