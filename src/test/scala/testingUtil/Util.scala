package testingUtil

import com.nathanielmay.minesweeper.{H, MineSweeper, Square, V}

object Util {

  implicit def hAble(i: Int): H = H(i)
  implicit def vAble(i: Int): V = V(i)

}
