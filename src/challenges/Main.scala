package challenges

import scala.annotation.tailrec

object Main1 extends App {

  val input = List(("CHI", "MAN"), ("ROM", "BOS"), ("CPH", "AMS"), ("MAN", "BOS"), ("BER", "AMS"), ("BER", "ROM"))

  def buildList(input: List[(String,String)]): List[String] = {

    def getNext(s: String, xs: List[(String, String)]): (List[(String, String)], String) =
      xs.foldLeft((List.empty[(String, String)], "")) { case ((l, next), (k, v)) =>
        if (k == s) {
          (l, v)
        } else if (v == s) {
          (l, k)
        } else {
          ((k, v) :: l, next)
        }
      }

    @tailrec
    def go(prev: String, xs: List[(String, String)], acc: List[String]): List[String] =
      if (xs.isEmpty) acc :+ prev else {
        val (m, next) = getNext(prev, xs)
        go(next, m, acc :+ prev)
      }

    if(input.nonEmpty) go(input.head._2, input.tail, Nil :+ input.head._1) else Nil

  }

  println(buildList(input))


}
