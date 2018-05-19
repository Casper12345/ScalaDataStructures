package linear_search

class LinearSearch {

  def linearSearch[A](xs: Array[A], i: A): Boolean = {
    def go(n: Int): Boolean = {
      if (n == xs.length) {
        false
      } else {
        if (xs(n) == i) {
          true
        } else {
          go(n = n + 1)
        }
      }
    }

    go(0)
  }


}

object Main extends App {

  val l = new LinearSearch
  println(l.linearSearch(Array(1, 2, 3, 4, 5, 8, 7, 9), 0))

}
