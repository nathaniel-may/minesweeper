package com.nathanielmay.minesweeper

//scalacheck
import org.scalacheck.{Gen, Prop, Properties}
import Prop.{BooleanOperators, exists, forAll}
import org.scalacheck.Gen.choose
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper

//scala
import Stream.Empty



object ShuffleProperties extends Properties("Shuffle") {

  property("results in exactly the same elements") = forAll {
    (s: Stream[Int], seed: Long) => frequencyMap(Shuffle.shuffle(seed)(s).toList) == frequencyMap(s.toList)
  }

  def frequencyMap[T](l: List[T]): Map[T, Int] =
    l.foldLeft[Map[T, Int]](Map()) { (m, t) => m.updated(t, m.getOrElse(t, 0) + 1) }

}
