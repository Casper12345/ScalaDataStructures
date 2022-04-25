package sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object InsertionSort extends App {

  def insertionSort[A: ClassTag](xs: List[A], f: (A, A) => Boolean): List[A] = {
    val arr = xs.toArray

    def swap(i1: Int, i2: Int): Unit = {
      val a = arr(i1)
      arr(i1) = arr(i2)
      arr(i2) = a
    }

    @tailrec
    def find(i: Int): Unit =
      if(i > 0 && !f(arr(i -1), arr(i))) {
        swap(i -1, i)
        find(i -1)
      }

    @tailrec
    def go(i: Int): Unit =
      if(i < arr.length){
         find(i)
         go(i + 1)
      }
    go(0)
    arr.toList
  }

  println(insertionSort[Int](List(5,4,3,2,1), _ < _ ))

}
