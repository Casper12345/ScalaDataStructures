package sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object ShellSort extends App {

  def shellSort[A: ClassTag](xs: List[A], f: (A, A) => Boolean): List[A] = {

    val arr = xs.toArray

    def swap(i1: Int, i2: Int): Unit = {
      val a = arr(i1)
      arr(i1) = arr(i2)
      arr(i2) = a
    }

    @tailrec
    def createSeq(h: Int = 1): Int = if (h < arr.length / 3) createSeq(3 * h + 1) else h

    @tailrec
    def iLoop(i: Int, h: Int): Unit = if(i < arr.length){
      jLoop(i, h)
      iLoop(i + 1, h)
    } else ()

    @tailrec
    def jLoop(j: Int, h: Int): Unit = if (j >= h && f(arr(j), arr(j-h))) {
      swap(j, j-h)
      jLoop(j - h, h)
    } else ()

    @tailrec
    def go(h: Int): Unit = if(h > 0){
        iLoop(h,h)
        go(h = h / 3)
    } else ()


    go(createSeq())

    arr.toList
  }

  println(shellSort[Int](List(7,10,5,3,8,100,45,4,2,9,6,1,34,11,2,9,8,1), _ < _))

}
