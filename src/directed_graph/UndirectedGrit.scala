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

    if(size > 0) init(size) else Map()

  }


  println(initGrit(2))

}
