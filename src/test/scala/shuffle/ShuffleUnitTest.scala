package shuffle

import org.scalatest._
import scalaz.Scalaz._
import shuffle.Shuffle.shuffle

import scala.math.abs
import scala.util.Random


class ShuffleUnitTest extends FlatSpec with Matchers {

  "A Shuffled Stream" should "have a roughly even distribution" in {
    val samples   = 120*120
    val size      = 3
    val tolerance = .05
    val input: Stream[Stream[Int]] = Stream.tabulate(samples)(_ => (0 until size).toStream)

    val map = (for {
      stream <- input
      key    =  shuffle(stream)//.toList
    } yield key)
      .sequence
      .eval(new Random(System.nanoTime()))
      .map { _.toList }
      .foldLeft[Map[List[Int], Int]](Map()) {
      (m,k) => m.updated(k, m.getOrElse(k, 0) + 1) }

//      for ((k,v) <- map)
//        println(s"$k: $v")

      map.values.forall { within(_, samples.toDouble/factorial(size), tolerance) } should be (true)
  }

  def within(value: Double, goal: Double, percent: Double): Boolean =
    if (value >= goal) goal + goal*abs(percent) > value
    else goal - goal*abs(percent) < value

  def factorial(n: Int): Int = (2 to n).product //TODO there must be a library function for this that uses BigDecimal

}
