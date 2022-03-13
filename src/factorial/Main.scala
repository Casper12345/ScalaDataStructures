package factorial

import scala.annotation.tailrec

object Main extends App {

  def factorial(i: Int): Int = {
    @tailrec
    def go(i: Int, acc: Int): Int = if(i > 1) go(i -1, acc * i) else acc
    go(i, 1)
  }

  println(factorial(9))


  def fibo(i: Int): Int = {
    @tailrec
    def go(i: Int, prev: Int, next: Int): Int =
      if(i < 1) prev else go(i -1, next, next + prev)
    go(i, 0, 1)

  }

  println(fibo(0))
  println(fibo(1))
  println(fibo(2))
  println(fibo(3))
  println(fibo(6))
  println(fibo(10))

}
