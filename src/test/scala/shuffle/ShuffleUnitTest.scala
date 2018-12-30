package shuffle

import org.scalatest._
import shuffle.Shuffle.shuffle

import scala.math.abs
import scala.util.Random


class ShuffleUnitTest extends FlatSpec with Matchers {

  "A Shuffled Stream" should "have a roughly even distribution" in {
    val size      = 3
    val tolerance = .05
    val samples   = scala.math.pow(2 * size / tolerance, 2).toInt
    val goal      = samples.toDouble/factorial(size)
    val input: Stream[Stream[Int]] = Stream.tabulate(samples)(_ => (0 until size).toStream)

    val map = (for {
      stream <- input
      key    =  shuffle(stream)
    } yield key)
      .foldLeft[Map[List[Int], Int]](Map()) {
        (m, rand) => {
          val k = rand.eval(new Random(System.nanoTime())).toList
          m.updated(k, m.getOrElse(k, 0) + 1) } }

      withClue(s"all values must be within (${min(goal, tolerance)}, ${max(goal, tolerance)})\n") {
        map.values.filter(notWithin(goal, tolerance)) should be (empty) }
  }

  def min(goal: Double, tol: Double): Double = goal - goal*abs(tol)
  def max(goal: Double, tol: Double): Double = goal + goal*abs(tol)

  def within(goal: Double, tol: Double)(value: Int): Boolean =
    if (value >= goal) max(goal, tol) > value
    else               min(goal, tol) < value

  def notWithin(goal: Double, tol: Double)(value: Int): Boolean = !within(goal, tol)(value)

  def factorial(n: Long): Long = (2L to n).product //TODO use library fn

}
