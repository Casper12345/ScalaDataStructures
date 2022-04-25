package graphs

import scala.annotation.tailrec

object Main extends App {

  val gr = Graph(100)

  gr.addEdge(3, 13)
  gr.addEdge(13, 23)
  gr.addEdge(23, 33)
  gr.addEdge(33, 43)
  gr.addEdge(43, 53)
  gr.addEdge(43, 44)
  gr.addEdge(44, 45)
  gr.addEdge(45, 55)
  gr.addEdge(55, 65)
  gr.addEdge(55, 65)
  gr.addEdge(65, 75)
  gr.addEdge(75, 85)
  gr.addEdge(85, 95)
  gr.addEdge(95, 94)
  gr.addEdge(94, 93)

  gr.addEdge(53, 63)
  gr.addEdge(63, 73)
  gr.addEdge(73, 83)
  gr.addEdge(83, 93)

  @tailrec
  def printOut(graph: Graph, i: Int): Unit =
    if(i < graph.size){
      graph.adj(i).foreach(s => println(i + "-" + s))
      printOut(graph, i + 1)
    }

  printOut(gr, 0)


  def matrixPrint(graph: Graph, partTo: List[Int]): Unit = {
    def fill(s: Int, e: Int): List[Int] =
      (for { i <- s to e} yield i).toList

    val numbers = for {
      i <- 1 to Math.sqrt(graph.size).toInt
    } yield {
      ((i * Math.sqrt(graph.size).toInt) - Math.sqrt(graph.size).toInt, (i * Math.sqrt(graph.size).toInt) - 1)
    }
    numbers.toList.foreach{ case (s,e) => fill(s,e)
      .map(i =>
        if(graph.adj(i).nonEmpty && partTo.contains(i)) "[@]" else if (graph.adj(i).nonEmpty) "[*]" else "[ ]").foreach(print); println
    }
  }

  matrixPrint(gr, breathFirstSearch(gr, 3, 93))


  private def pathTo(s: Int, v: Int, edgeTo: List[Int]): List[Int] = {
    @tailrec
    def go(i: Int, acc: List[Int]): List[Int] = {
      if(i == s) acc else {
        go(edgeTo(i), i :: acc)
      }
    }
    go(v, Nil)
  }

  def depthFirstSearch[A](gr: Graph, s: Int, v: Int): List[Int] = {
    val visited: Array[Boolean] = Array.fill(gr.size)(false)
    val edgeTo: Array[Int] = Array.fill(gr.size)(0)


    def go(gr: Graph, v: Int): Unit = {
      visited(v) = true
      @tailrec
      def traverseEdges(edges: List[Int]): Unit = edges match {
        case h :: t =>
          if (!visited(h)) {
            edgeTo(h) = v
            go(gr, h)
            traverseEdges(t)
          }
        case Nil => ()
      }
      traverseEdges(gr.adj(v))
    }

    go(gr, s)

    if(visited(v)) s :: pathTo(s, v, edgeTo.toList) else Nil
  }

  def breathFirstSearch(gr: Graph, s: Int, v: Int): List[Int] = {
    val visited: Array[Boolean] = Array.fill(gr.size)(false)
    val edgeTo: Array[Int] = Array.fill(gr.size)(0)
    var q: List[Int] = Nil

    def enqueue(i: Int): Unit = q = i :: q
    def dequeue: Int = {
      val result = q.last
      q = q.dropRight(1)
      result
    }

    enqueue(s)
    visited(s) = true

    @tailrec
    def go(): Unit = {
        val v = dequeue
        @tailrec
        def traverseEdges(edges: List[Int]): Unit = edges match {
          case h :: t =>
            if (!visited(h)) {
              enqueue(h)
              visited(h) = true
              edgeTo(h) = v
            }
            traverseEdges(t)
          case Nil => ()
        }
        traverseEdges(gr.adj(v))

      if (q.nonEmpty) go()
    }

    go()

    if(visited(v)) s :: pathTo(s, v, edgeTo.toList) else Nil
  }

}

case class Graph(size: Int){
  var edges: Array[List[Int]] = Array.fill(size)(Nil)
  def addEdge(v: Int, w: Int): Unit = {
    edges(v) = w :: edges(v)
    edges(w) = v :: edges(w)
  }
  def adj(i: Int): List[Int] = edges(i)
}


