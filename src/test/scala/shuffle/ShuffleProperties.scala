package shuffle

//scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Prop, Properties}

//scala

//project
import shuffle.Shuffle.shuffle



object ShuffleProperties extends Properties("shuffle") {

  property("results in exactly the same elements") = forAll {
    (s: Stream[Int], seed: Long) => frequencyMap(shuffle(seed)(s).toList) == frequencyMap(s.toList)
  }

  property("halves a stream") = forAll {
    s: Stream[Int] => isHalf(Shuffle.halve(s))
  }

  def frequencyMap[T](l: List[T]): Map[T, Int] =
    l.foldLeft[Map[T, Int]](Map()) { (m, t) => m.updated(t, m.getOrElse(t, 0) + 1) }

  def isHalf[T](ss: (Stream[T], Stream[T])): Boolean = ss match {
    case (s1, s2) => (s1.size - s2.size) == 1 ||
                     (s1.size - s2.size) == 0
  }

}
