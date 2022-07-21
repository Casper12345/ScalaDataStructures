package quick_sort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

object QuickSort {

  private def partition[A](xs: Array[A], lo: Int, hi: Int)(f: (A,A) => Boolean): Int = {

    @tailrec
    def mvI(i: Int): Int = if (i == hi || !f(xs(i), xs(lo))) i else mvI(i + 1)
    @tailrec
    def mvJ(j: Int): Int = if (j == lo || !f(xs(lo), xs(j))) j else mvJ(j - 1)

    @tailrec
    def go(a: Int, b: Int): Int = {
      val i = mvI(a + 1)
      val j = mvJ(b - 1)

      if (i >= j) {
        exchange(xs, lo, j)
        j
      } else {
        exchange(xs, i, j)
        go(i,j)
      }
    }
    go(lo, hi+1)
  }

  private def exchange[A](xs: Array[A], lo: Int, hi: Int): Unit = {
    val current = xs(lo)
    xs(lo) = xs(hi)
    xs(hi) = current
  }

  private def sort[A](xs: Array[A], lo: Int, hi: Int)(f: (A,A) => Boolean): Unit = {
    if(hi > lo){
      val j = partition(xs, lo, hi)(f)

      sort(xs, lo, j - 1)(f)
      sort(xs, j + 1, hi)(f)
    }

  }

  def sort[A : ClassTag](xs: List[A])(f: (A,A) => Boolean): List[A] = {
    val a = Random.shuffle(xs).toArray
    sort(a, 0, a.length -1)(f)
    a.toList
  }

}

object Main extends App {
  println(QuickSort.sort[Int](List(9,8,7,6,5,7,2,1,0,-1,20,44,99))(_ < _))
}