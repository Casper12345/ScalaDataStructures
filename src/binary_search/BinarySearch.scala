package binary_search

trait BinarySearchSecond {


  def binarySearch[A](xs: Array[A], i: A)(f: (A, A) => Boolean): Boolean = {

    if (xs.length < 2) {
      return false
    }

    if (xs(xs.length / 2) == i) {
      true
    } else {
      val (a1, a2) = xs.splitAt(xs.length / 2)

      if (f(i, xs(xs.length / 2))) {
        binarySearch(a1, i)(f)
      } else {
        binarySearch(a2, i)(f)
      }
    }
  }

}

object Main2 extends App with BinarySearchSecond {

  println(binarySearch(Array(1, 2, 3, 4, 5, 6, 7), 3)((a, b) => a < b))

}
