package bubble_sort

import scala.annotation.tailrec

object BubbleSort extends App {

  @tailrec
  def bubbleSort[A](xs: List[A])(f: (A, A) => Boolean): List[A] = {
    @tailrec
    def helper(xs: List[A], acc: List[A], isSorted: Boolean): (List[A], Boolean) = xs match {
      case Nil => (acc, isSorted)
      case h :: Nil => helper(Nil, acc :+ h, isSorted)
      case h :: t =>
        if (f(h, t.head)) {
          helper(h :: t.tail, acc :+ t.head, isSorted = true)
        } else {
          helper(t, acc :+ h, isSorted)
        }
    }

    val (l, b) = helper(xs, Nil, isSorted = false)
    if (b) {
      bubbleSort(l)(f)
    } else {
      l
    }

  }

  println(bubbleSort(List(1, 2, 3, 4, 5, 6, 7, 8).reverse)((a: Int, b: Int) => a > b))

}
