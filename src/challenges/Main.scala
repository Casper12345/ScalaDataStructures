package challenges

import scala.annotation.tailrec
import scala.collection.mutable

object Main1 extends App {

  val input = List(("CHI", "MAN"), ("ROM", "BOS"), ("CPH", "AMS"), ("MAN", "BOS"), ("BER", "AMS"), ("BER", "ROM"))

  // squared running time
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


  // linear running time
  def buildList2(input: List[(String,String)]): List[String] = {
    val m = input.foldLeft(Map.empty[String, List[String]]) { case (m, (k, v)) =>
      if (m.contains(k) && m.contains(v)) {
        m + ((k, v :: m(k))) + ((v, k :: m(v)))
      } else if (m.contains(k)) {
        m + ((k, v :: m(k))) + ((v, k :: Nil))
      } else if (m.contains(v)) {
        m + ((v, k :: m(v))) + ((k, v :: Nil))
      } else {
        m + ((k, v :: Nil)) + ((v, k :: Nil))
      }
    }

    def dfs(
      v: String,
      visited: mutable.Map[String, Boolean],
      edgeTo: mutable.Map[String, String]
    ): Map[String, String] = {

      visited.put(v, true)
      val edges = m(v)

      @tailrec
      def traverseEdges(e: List[String]): Unit = e match {
        case h :: t =>
          if (!visited(h)) {
            edgeTo.put(v, h)
            dfs(h, visited, edgeTo)
          }
          traverseEdges(t)
        case Nil => ()
      }

      traverseEdges(edges)
      edgeTo.toMap
    }

    lazy val returnMap =
      dfs(
        input.head._1,
        mutable.Map[String, Boolean](m.keys.map { k => (k, false) }.toSeq: _*),
        mutable.Map[String, String]()
      )

    @tailrec
    def getResult(next: String, acc: List[String]): List[String] =
      if (returnMap.contains(next)) {
        getResult(returnMap(next), acc :+ next)
      } else {
        acc :+ next
      }

    if(input.nonEmpty) getResult(input.head._1, Nil) else Nil
  }


  println(buildList2(input))


}
