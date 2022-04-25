package sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object SelectionSort extends App {

  def sort[A: ClassTag](xs: List[A], f: (A, A) => Boolean): List[A] = {
    val arr = xs.toArray

    def swap(i1: Int, i2: Int): Unit = {
      val a = arr(i1)
      arr(i1) = arr(i2)
      arr(i2) = a
    }

    @tailrec
    def find(i: Int, index: Int): Int =
      if (i > arr.length - 1) index else {
        if (f(arr(index), arr(i))) find(i + 1, i) else find(i + 1, index)
      }

    @tailrec
    def go(i: Int): Unit =
      if (i < arr.length) {
        val index = find(0, i)
        println(index)
        if (!f(arr(index), arr(i))) {
          swap(index, i)
          go(i + 1)
        }
      }

    go(0)
    arr.toList
  }


  println(sort[Int](List(7, 6, 5, 4, 3, 2, 1), _ < _))

}
