package shuffle

import scalaz.{Monad, State}

import scala.Stream.Empty
import scala.collection.mutable
import scala.util.Random

object Shuffle {

  def main(args: Array[String]): Unit = {
    def pretty[K, V](m: mutable.Map[K, V]): String =
      m.iterator.map { case (k, v) => s"$k -> $v" }
        .mkString("\n")

    var m: mutable.Map[List[Int], Int] = mutable.Map()

    for (_ <- 1 to 1000) {
      val seq = shuffle(System.nanoTime())(Stream(1,2,3,4)).toList
      m.update(seq, m.getOrElse(seq, 0) + 1)
    }

    println(pretty(m))

    var rngMap: mutable.Map[Int, Int] = mutable.Map()
    for (_ <- 1 to 1000) {
      val r = rng(6).eval(new Random(System.nanoTime()))
      rngMap.update(r, rngMap.getOrElse(r, 0) + 1)
    }

    println(pretty(rngMap))
  }

  type Rand[A] = State[Random, A]

  def shuffle[A](seed: Long)(s: Stream[A]): Stream[A] = shuffleLength(s).eval(new Random(seed))._1

  def shuffle[A](s: Stream[A]): Rand[Stream[A]] = shuffleLength(s).map(_._1)

  /*private[shuffle]*/ def halve[A](s: Stream[A]): (Stream[A], Stream[A]) = s match {
    case Empty    => (Empty, Empty)
    case z #:: zs => halve(zs) match { case (xs, ys) => (z #:: ys, xs) }
  }

  /*private[shuffle]*/ def shuffleLength[A](s: Stream[A]): Rand[(Stream[A], Int)] = s match {
    case Empty       => Monad[Rand].point((Empty,     0))
    case x #:: Empty => Monad[Rand].point((Stream(x), 1))
    case _ #:: _     => halve(s) match { case (lHalf, rHalf) => for {
      left           <- shuffleLength(lHalf)
      right          <- shuffleLength(rHalf)
      (lSize, rSize) =  (left._2, right._2)
      shuffled       <- riffle(left, right)
    } yield (shuffled, lSize + rSize) }
  }

  /*private[shuffle]*/ def riffle[A](l: (Stream[A], Int), r: (Stream[A], Int)): Rand[Stream[A]] = (l, r) match {
    case ((xs,       _ ), (Empty,    _ )) => Monad[Rand].point(xs)
    case ((Empty,    _ ), (ys,       _ )) => Monad[Rand].point(ys)
    case ((x #:: xs, nx), (y #:: ys, ny)) => rng(below = nx+ny).flatMap { k =>
      if (k < nx) riffle((xs,       nx-1), (y #:: ys, ny))   map { x #:: _ }
      else        riffle((x #:: xs, nx),   (ys,       ny-1)) map { y #:: _ }
    }
  }

  //[0, below) TODO deal with bad input better?
  def rng(below: Int): Rand[Int] = for {
        r <- State.get[Random]
        i =  if (below <= 0 ) 1 else below // zeros for bad input
        b =  r.nextInt(i) // throws when i <= zero
      } yield b
}