package com.nathanielmay.minesweeper

import org.scalatest._
import scalaz.Scalaz, Scalaz._ //unfold
import Shuffle.shuffle
import scala.math.abs


class ShuffleUnitTest extends FlatSpec with Matchers {

  "A Shuffled Stream" should "have a roughly even distribution" in {
    val samples   = 1000
    val size      = 3
    val tolerance = .05
    val input: Stream[Stream[Int]] = unfold(samples)(count => if (count <= 0) None else Some(((0 until size).toStream, count - 1)))

    val map = (for {
      stream <- input
      key    =  shuffle(System.nanoTime())(stream).toList
    } yield key).foldRight[Map[List[Int], Int]](Map()) {
      case (k, m) => m.updated(k, m.getOrElse(k, 0) + 1) }

//      for {
//        i <- map.values
//        _ =  println(s"val: $i")
//      } yield ()

      map.values.forall { within(_, samples.toDouble/factorial(size), tolerance) } should be (true)
  }

  def within(value: Double, goal: Double, percent: Double): Boolean =
    if (value >= goal) goal + goal*abs(percent) > value
    else goal - goal*abs(percent) < value

  def factorial(n: Int): Int = (2 to n).product //TODO there must be a library function for this that uses BigDecimal

}
