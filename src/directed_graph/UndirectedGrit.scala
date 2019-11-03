package directed_graph

import scala.collection.mutable

object UndirectedGrit extends App {

  def initGrit(size: Int): Map[(Int, Int), List[(Int, Int)]] = {

    def init(size: Int): Map[(Int, Int), List[(Int, Int)]] = {

      val m: mutable.Map[(Int, Int), List[(Int, Int)]] = mutable.Map()

      val xSize = size - 1
      val ySize = size - 1

      var x = 0
      while (x < xSize + 1) {
        var y = 0
        while (y < ySize + 1) {

          if (x == 0 && y == 0) {
            m += (((x, y), List((x + 1, y), (x, y + 1), (x + 1, y + 1))))
          } else if (y == ySize && x == xSize) {
            m += (((x, y), List((x - 1, y), (x, y - 1), (x - 1, y - 1))))
          } else if (x == xSize && y == 0) {
            m += (((x, y), List((x - 1, y), (x, y + 1), (x - 1, y + 1))))
          } else if (y == ySize && x == 0) {
            m += (((x, y), List((x + 1, y), (x, y - 1), (x + 1, y - 1))))
          } else if (y == 0) {
            m += (((x, y), List((x, y + 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x + 1, y + 1))))
          } else if (x == 0) {
            m += (((x, y), List((x + 1, y), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y + 1))))
          } else if (y == ySize) {
            m += (((x, y), List((x, y - 1), (x - 1, y), (x + 1, y), (x - 1, y - 1), (x + 1, y - 1))))
          } else if (x == xSize) {
            m += (((x, y), List((x - 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1))))
          } else {
            m += (((x, y), List(
              (x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y), (x - 1, y + 1), (x - 1, y - 1), (x + 1, y + 1), (x + 1, y - 1)))
              )
          }
          y = y + 1
        }
        x = x + 1
      }

      if (size == 1) Map(((0, 0), Nil)) else m.toMap
    }

    if (size > 0) init(size) else Map()

  }

  def getEuclideanDistance(a: (Int, Int), b: (Int, Int)): Double = Math.sqrt(
    Math.pow(b._1.toDouble - a._1.toDouble, 2) + Math.pow(b._2.toDouble - a._2.toDouble, 2)
  )

  def eu(a: (Int, Int), b: (Int, Int)): Double = {
    val dx = Math.abs(a._1 - b._1)
    val dy = Math.abs(a._2 - b._2)
    Math.sqrt(dx * dx + dy * dy)
  }

  def getManhattanDistance(a: (Int, Int), b: (Int, Int)): Double =
    Math.abs(a._1.toDouble - b._1.toDouble) + Math.abs(a._2.toDouble - b._2.toDouble)


  def dijkstra(graph: Map[(Int, Int), List[(Int, Int)]], src: (Int, Int), dest: (Int, Int)): List[(Int, Int)] = {

    def getShortestDist(l: List[(Int, Int)], m: Map[(Int, Int), Double]): ((Int, Int), List[(Int, Int)]) = {
      val sorted = l.map(a => a -> m(a)).sortBy { case (_, d) => d }.map { case (x, _) => x }
      (sorted.head, sorted.tail)
    }

    def loop(q: List[(Int, Int)], acc: mutable.Map[(Int, Int), (Double, (Int, Int))]): Map[(Int, Int), (Int, Int)] = q match {
      case Nil => acc.toMap.map { case (x, (_, p)) => (x, p) }
      case l =>
        val (u, r) = getShortestDist(l, acc.toMap.map { case (x, (d, _)) => (x, d) })
        graph(u).foreach { v =>
          val (dv, _) = acc(v)
          val alt = eu(v, dest) + eu(u, v)
          if (alt < dv) {
            acc.put(v, (alt, u))
          }
        }
        loop(r, acc)
    }

    def findPath(p: (Int, Int), d: (Int, Int), acc: List[(Int, Int)], m: Map[(Int, Int), (Int, Int)]): List[(Int, Int)] = p match {
      case x if x == d => acc
      case _ => findPath(m(p), d, acc :+ m(p), m)
    }

    val distanceAndPrev: mutable.Map[(Int, Int), (Double, (Int, Int))] = mutable.Map() ++ graph.keys.map {
      case x if x == dest => x -> (0.0, (-1, -1))
      case x => x -> (Double.MaxValue, (-1, -1))
    }.toMap

    val m = loop(graph.keys.toList, distanceAndPrev)

    src :: findPath(src, dest, Nil, m)


  }

  def createMatrix(l: List[(Int, Int)], size: Int): Array[Array[String]] = {
    val m = Array.ofDim[String](size,size)
    var x = 0
    while (x < size) {
      var y = 0
      while (y < size) {
        if (l.contains((x, y))) m(x)(y) = "x" else m(x)(y) = " "
        y = y + 1
      }
      x = x + 1
    }
    new Rotate().rotateMatrixBy90DegreeCounterClockwise(m)
  }


  def printMatrix(l: List[(Int, Int)], size: Int): Unit = {
    val a = createMatrix(l,size)
    var x = 0
    println("--" * size)
    while (x < size) {
      var y = 0
      while (y < size) {
        print("|" + a(x)(y))
        y = y + 1
      }
      println("|")
      println("--" * size)
      x = x + 1
    }
  }

//    println(getManhattanDistance((0,0), (1,1)))
//    println(getEuclideanDistance((0,0), (1,1)))

  printMatrix( dijkstra(initGrit(10), (8, 8), (9, 9)), 10 )


}
