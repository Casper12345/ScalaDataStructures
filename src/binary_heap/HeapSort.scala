package binary_heap

import scala.annotation.tailrec
import scala.reflect.ClassTag

class HeapSort[A: ClassTag](f: (A,A) => Boolean) {

  def sort(input: List[A]): List[A] = {
    val a = toArray(input)

    @tailrec
    def heapify(k: Int, size: Int): Unit =
      if(k >= 1) {
        sink(a, k, size)
        heapify(k - 1, size)
      } else ()

    heapify((a.length - 1) / 2, a.length - 1)

    @tailrec
    def exec(i: Int): Unit =
      if(i > 1) {
        swap(a, 1, i)
        sink(a, 1, i - 1)
        exec(i - 1)
      } else ()
    exec(a.length - 1)
    toList(a)
  }

  private def toArray(xs: List[A]): Array[A] = {
    val a = new Array[A](xs.length + 1)
    @tailrec
    def go(i: Int): Unit =
      if(i < xs.length){
        a(i+1) = xs(i)
        go(i + 1)
      }
    go(0)
    a
  }

  private def toList(a: Array[A]): List[A] = a.tail.toList

  private def swap(a: Array[A], x: Int, y: Int): Unit = {
    val buffer = a(y)
    a(y) = a(x)
    a(x) = buffer
  }

  private def sink(a: Array[A], k: Int, size: Int): Unit = {
    @tailrec
    def go(k: Int): Unit =
      if(2 * k <= size) {
        val j = if(2*k + 1 <= size && f(a(2*k), a(2*k+1))) 2*k+1 else 2*k
        if(!f(a(k), a(j))) () else {
          swap(a, k, j)
          go(j)
        }
      } else ()
    go(k)
  }

}

object Main2 extends App {
  val sort = new HeapSort[Int](_ < _)

  println(sort.sort(List(5,2,6,3,4,8,7,9,1,10)))


}