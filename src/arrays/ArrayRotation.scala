package arrays

import scala.annotation.tailrec
import scala.reflect.ClassTag

object ArrayRotation extends App {


  def rotate[A](a: Array[A], d: Int)(implicit m: ClassTag[A]): Array[A] = {

    @tailrec
    def copy(i1: Int, i2: Int, s: Int, a1: Array[A], a2: Array[A]): Unit  = {
      if(i2 != s) {
        a1(i1) = a2(i2)
        copy(i1 + 1,i2 + 1, s, a1, a2)
      }
    }

    @tailrec
    def move(i: Int, p: Int): Unit = {
      if(p != a.length){
        a(i) = a(p)
        move(i + 1, p + 1)
      }
    }

    if(d > 0 || d <= a.length){
      val temp = new Array[A](d)
      copy(0,0, temp.length, temp, a)
      move(0, d)
      copy(a.length - d, 0, temp.length, a, temp)
      a
    } else {
      a
    }

  }


  rotate(Array(1,2,3,4,5,6,7,8,9,10), 10).foreach(println)

}