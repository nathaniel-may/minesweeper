package com.nathanielmay.minesweeper

import org.scalatest._
import fs2.{Stream, Pure}
import Shuffle.fs2Shuffle
import scala.math.abs


object ShuffleUnitTest extends FlatSpec with Matchers {

  "A Shuffled Stream" should "have a roughly even distribution" in {
    type PureStream[T] = fs2.Stream[Pure, T]
    val samples   = 1000
    val size      = 10
    val tolerance = .05
    val input: PureStream[PureStream[Int]] = Stream.unfold(samples)(count => if (count <= 0) None else Some((Stream.emits(0 until size), count - 1)))

    (for {
      stream <- input
      key    =  fs2Shuffle(System.nanoTime())(stream).toList
    } yield key).fold[Map[List[Int], Int]](Map()) {
      case (m, k) => m.updated(k, m.getOrElse(k, 0) + 1) }
      .compile
      .toList
      .head
      .values.forall { within(_, samples/factorial(size), tolerance) } should be true
  }

  def within(value: Int, goal: Int, percent: Double): Boolean =
    goal - goal*abs(percent) < value || goal + goal*abs(percent) > value

  def factorial(n: Int): Int = (2 to n).product //TODO there must be a library function for this that uses BigDecimal

//  (sample: Stream[(Long, Stream[Int])]) => { //TODO must be streams of unique elements
//    def go(m: Map[List[Int], Int], s: Stream[(Long, Stream[Int])], len: Int): (Map[List[Int], Int], Int) = s match {
//      case empty    => (m, 0)
//      case x :: xs =>
//        val (seed, stream) = x
//        val shuffled = shuffle(seed)(stream).toList
//        go(m.updated(shuffled, m.getOrElse(shuffled, 0) + 1), xs, len+1)
//    }
//
//    val e = .05
//    go(Map(), sample, 0) match {
//      case (m, samples) => m.values.forall(within(_, samples/factorial(size), e))
//    }
//
//
//  }
}
