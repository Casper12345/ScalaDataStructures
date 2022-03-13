package arrays

import scala.annotation.tailrec
import scala.reflect.ClassTag

object LinearRecursion extends App {


  def linearSum(a: Array[Int]): Int = {
    @scala.annotation.tailrec
    def go(i: Int, acc: Int): Int = {
      if(i == a.length) acc else go(i + 1, acc + a(i))
    }
    go(0, 0)
  }

  println(linearSum(Array(1,2,3,4)))

  def reverse[A : ClassTag](a: Array[A]): Array[A] = {
    @scala.annotation.tailrec
    def go(low: Int, high: Int, a: Array[A]): Array[A] =
      if(low > high) a else {
        val b = a(low)
        a(low) = a(high)
        a(high) = b
        go(low + 1, high - 1, a)
      }
   go(0, a.length - 1, a)
  }


  def pow(i: Int, p: Int): Int = {
    @tailrec
    def go(count: Int, acc: Int): Int =
      if(count == 0) acc else go(count - 1, acc * i)
    go(p, 1)
  }

  println(pow(5, 2))


  def largestElement[A](a: Array[A], f: (A,A) => Boolean): Option[A] = {
    @tailrec
    def go(i: Int, b: Option[A]): Option[A] =
      if (i == a.length) b else {
        val buf = b match {
          case None => Some(a(i))
          case Some(v) => if(f(a(i), v)) Some(a(i)) else Some(v)
        }
        go(i + 1, buf)
      }

    go(0, None)
  }

  println(largestElement[Int](Array(9,2,3,1,18,1), _ > _))
}
