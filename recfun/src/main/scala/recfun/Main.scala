package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row)+" ")
      println()
    }
    println("\"(if (zero? x) max (/ 1 x))\"==="+balance("(if (zero? x) max (/ 1 x))" toList))
    println("\"I told him (that it’s not (yet) done). (But he wasn’t listening)\"==="+balance("I told him (that it’s not (yet) done). (But he wasn’t listening)" toList))
    println("\":-)\"==="+balance(":-)" toList))
    println("\"())(\"==="+balance("())(" toList))
    println(countChange(4, List(1, 2)))
  }

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
      def balanceStep(chars: List[Char], opened: Int): Boolean =
        if (opened < 0) false
        else if (chars isEmpty) opened == 0
        else chars head match {
          case '(' => balanceStep(chars tail, opened + 1)
          case ')' => balanceStep(chars tail, opened - 1)
          case _ => balanceStep(chars tail, opened)
        }
    balanceStep(chars, 0)
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money <0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins tail)
}
