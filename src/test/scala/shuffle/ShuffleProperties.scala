package shuffle

//scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, Gen}, Gen.choose
import scalaz._, Scalaz._

//Scala
import scala.util.Random

//project
import shuffle.Shuffle.shuffle



object ShuffleProperties extends Properties("shuffle") {

  property("results in exactly the same elements") = forAll {
    (s: Stream[Int], seed: Long) => (for { //frequencyMap(shuffle(s).toList) == frequencyMap(s.toList)
      rand <- shuffle(s)
    } yield frequencyMap(rand.toList) == frequencyMap(s.toList))
    .eval(new Random(seed))
  }

  property("halves a stream") = forAll {
    s: Stream[Int] => isHalf(Shuffle.halve(s))
  }

  property("riffle maintains exactly the same elements") = forAll {
    (s1: Stream[Int], s2: Stream[Int], seed: Long) =>
      arePartitions(Shuffle.riffle((s1, s1.length), (s2, s2.length)).eval(new Random(seed))
        .foldLeft[List[Int]](List()) { (b, a) => a :: b })(s1.toList, s2.toList)
  }

  property("rng(below: Int > 0) generates only [0, below)") = forAll(choose(1, Int.MaxValue), choose(Long.MinValue, Long.MaxValue)) {
    (below: Int, seed: Long) => {
      val gen = Shuffle.rng(below).eval(new scala.util.Random(seed)).intValue
      gen < below && gen >= 0
    }
  }

  property("rng(below: Int <= 0) is always zero") = forAll(choose(Int.MinValue, 0), choose(Long.MinValue, Long.MaxValue)) {
    (below: Int, seed: Long) => Shuffle.rng(below).eval(new scala.util.Random(seed)).intValue == 0
  }

  property("riffling two single item lists isn't always the same") = forAll {
    seed: Long => List.fill(10)(Shuffle.riffle((Stream("A"), 1), (Stream("B"), 1)))
      .sequence
      .map { _.map { _.head } }
      .map(frequencyMap)
      .map { !_.values.exists(_ == 0) }
      .eval(new scala.util.Random(seed)).booleanValue
  }

  def arePartitions[T](full: List[T])(ls: List[T]*): Boolean =
    frequencyMap(full) == frequencyMap(ls.toList.flatten)

  def frequencyMap[T](l: List[T]): Map[T, Int] =
    l.foldLeft[Map[T, Int]](Map()) { (m, t) => m.updated(t, m.getOrElse(t, 0) + 1) }

  def isHalf[T](ss: (Stream[T], Stream[T])): Boolean = ss match {
    case (s1, s2) => (s1.size - s2.size) == 1 ||
                     (s1.size - s2.size) == 0
  }

}
