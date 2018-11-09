package com.nathanielmay.minesweeper

import scala.util.Random
import scalaz.State

object Util {

  def shuffle[A](seed: Long)(l: List[A]): List[A] = {
    split(l) match {
      case (l: List[A], r: List[A]) => riffle(seed)()
    }
  }

  private def split[A](list: List[A], l: List[A] = List(), r: List[A] = List()): (List[A], List[A]) = l match {
    case x :: xs => split(xs, x :: r, l)
    case Nil     => (l, r)
  }

  //TODO not calculating sizes right
  private def riffle[A](seed: Long)(l: (List[A], Int), r: (List[A], Int), out: List[A] = List()): List[A] = (l, r) match {
    case ((Nil, lSize),        (right :: rs, rSize)) => riffle(seed)((Nil, lSize), (rs, rSize), right :: out)
    case ((left :: ls, lSize), (Nil, rSize))         => riffle(seed)((ls,  lSize), (Nil, rSize), left :: out)
    case ((left :: ls, lSize), (right :: rs, rSize)) => rng(lSize + rSize).run(Random(seed)) match {
      case (r: Random, i: Int) if i < lSize => riffle(seed)((ls,  lSize - 1), (right :: rs, rSize), left :: out)
      case (r: Random, i: Int)              => riffle(seed)((left :: ls,  lSize), (rs, rSize - 1), right :: out)
    }
  }

  //[0, below)
  private def rng(below: Int): State[Random, Int] = for {
      r <- State.get[Random]
      i <- if (below <= 0 ) 1 // zeros for bad input
      b =  r.nextInt(i) // throws when i <= zero
    } yield b

}
