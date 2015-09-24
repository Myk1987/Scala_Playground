package recfun

import cafesat.api.Formulas.False
import common._
import math.abs

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle");
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(countChange(300, List(5, 10, 20, 50, 100, 200, 500)))

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c > r || c < 0 || r < 0) 0
    else if (c == 0 && r == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def partial(chars: List[Char], status: Int): Boolean =
      chars match {
        case '(' :: tail => partial(tail, status + 1)
        case ')' :: tail => if (status > 0) partial(tail, status - 1) else false
        case _ :: tail => partial(tail, status)
        case Nil => status == 0
      }
    partial(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 0
    else if (coins.isEmpty) 0
    else if (money == coins.head )  1 + countChange(money, coins.tail)
    else if (coins.head > money)    countChange(money, coins.tail)
    else if (2*coins.head <= money) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else if (coins.head < money)    countChange(money - coins.head, coins.tail) + countChange(money, coins.tail)
    else 0
}
