package quickSort

import scala.annotation.tailrec
import scala.reflect.ClassTag

object QuickSort extends App {



  def partition(xs: List[Int], l: Int, h: Int): List[Int] = {
    val pivot = xs(l)
    val leftWall = l

    @tailrec
    def go(i: Int, acc: List[Int]): List[Int] = {
      if(i < h){
        if(xs(i) < pivot){
          go(i +1, swap(xs, i, leftWall))
        } else {
          go(i +1, acc)
        }
      } else {
        acc
      }
    }

    swap(go(l +1, Nil), l, leftWall)



  }

  def swap[A : ClassTag](xs: List[A], i1: Int, i2: Int): List[A] = { // [1,2,3,4,5,6] (2,4)
   val a = xs.toArray
   a(i1) = a(i2)
   a(i2) = xs(i1)
   a.toList
  }

  println(swap(List(1,2,3,4,5,6), 5, 0))

}
