package com.nathanielmay.minesweeper

import scalaz._, Scalaz._

import scala.util.Random

object Util {

  type Rng[A] = State[Random, A]

  def Rng(n: Int): Rng[Int] = State[Random, Int](r => (r, r.nextInt(n)))

  //same sig as List.tabulate[Int]
  def rand(n: Int)(f: Int => Int): List[Int] =
    List.tabulate(n)(x => Rng(f(x)))
      .sequence[Rng, Int]
      .eval(new Random(System.currentTimeMillis()))
}
