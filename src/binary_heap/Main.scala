package binary_heap

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Main extends App {

  val maxHeap = new BinaryHeap[Int](_ < _)

   maxHeap.insert(1)
   maxHeap.insert(2)
   maxHeap.insert(3)
   maxHeap.insert(4)
   maxHeap.insert(10)
   maxHeap.print



}


class BinaryHeap[A: ClassTag](f: (A,A) => Boolean)  {

  private val a = new Array[A](10)

  def print: Unit =
    a.foreach(println)

  private var size = 0

  private def swim(k: Int): Unit = {
    @tailrec
    def go(k: Int): Unit =
      if(k > 1 && f(a(k / 2), a(k))){
        swap(a, k / 2, k)
        go(k / 2)
      } else ()
    go(k)
  }

  private def swap(a: Array[A], x: Int, y: Int): Unit = {
    val buffer = a(y)
    a(y) = a(x)
    a(x) = buffer
  }

  def insert(i: A): Unit = {
    size =  size + 1
    a(size) = i
    swim(size)
  }
}


