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

object Main2 extends App {


  def findDegree(a: Array[Int]): Int = {

    val count = Array.fill(a.max + 1)(0)
    val m = mutable.Map[Int, (Int, Int)]()

    @scala.annotation.tailrec
    def buildMap(i: Int): Unit =
      if (i < a.length) {
        count(a(i)) = count(a(i)) + 1
        if(m.contains(a(i))){
          m.put(a(i), (m(a(i))._1, i))
        } else {
          m.put(a(i), (i, i))
        }
        buildMap(i + 1)
      }

    buildMap(0)

    @tailrec
    def findHighest(i: Int, max: Int, buf: List[Int]): List[Int] =
      if(i < count.length) {
        if(count(i) == max){
          findHighest(i + 1, max, i :: buf)
        } else findHighest(i + 1, max, buf)
      } else buf

    @tailrec
    def getDegree(xs: List[Int], acc: Int): Int = xs match {
      case h :: t =>
        val (s,e) = m(h)
        val length = e - s + 1
        if(length > acc){
          getDegree(t, length)
        } else {
          getDegree(t, acc)
        }
      case Nil => acc
    }

    getDegree(findHighest(0, count.max, Nil), 0)

  }

  println(findDegree(Array(1,2,3,4,5,6,1,2,2)))



}
