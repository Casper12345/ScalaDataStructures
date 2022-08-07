package sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object SelectionSort extends App {

  def sort[A: ClassTag](xs: List[A], f: (A, A) => Boolean): List[A] = {
    val a = xs.toArray

    def swap(i1: Int, i2: Int): Unit = {
      val acc = a(i1)
      a(i1) = a(i2)
      a(i2) = acc
    }

    @tailrec
    def find(i: Int, j: Int, acc: Option[Int] = None): Unit =
      if(j == a.length) {
        acc match {
          case Some(index) => swap(i,index)
          case None => ()
        }
      } else {
        if(f(a(j), a(i))) {
          acc match {
            case Some(index) =>
              if (f(a(j), a(index))) find(i, j+1, Some(j)) else find(i, j+1, acc)
            case None => find(i, j+1, Some(j))
          }
        } else find(i, j+1, acc)
      }

    @tailrec
    def go(i: Int): Unit =
      if(i < a.length){
        find(i, i + 1)
        go(i +1)
      }

    go(0)
    a.toList
  }


  println(sort[Int](List(7,10,5,3,8,4,2,9,6,1), _ < _))

}
