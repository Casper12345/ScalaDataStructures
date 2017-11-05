package binarySearch


object BinarySearch {
  def apply() =
    new BinarySearch
}

class BinarySearch {

  def search[A <: Ordered[A]](lst: List[A], index: A): Boolean = {
    def helper(lst: List[A]): Boolean = {
      if (lst(lst.size / 2) == index) {
        return true
      }

      if (lst.size / 2 == 0) {
        false
      } else {
        val split = lst.splitAt(lst.size / 2)
        if (lst(lst.size / 2) > index)
          helper(split._2)
        else
          helper(split._1)
      }
    }

    helper(lst)
  }


}

case class Box[A](id: A) extends Ordered[Box[A]] {
  override def compare(that: Box[A]): Int = {
    if (id.hashCode() > that.id.hashCode()) {
      1
    } else if (id.hashCode() < that.id.hashCode()) {
      -1
    } else {
      0
    }
  }
}

object Main extends App {

  val b = BinarySearch.apply()

  println(b.search(List[Box[String]](Box("a"), Box("b")), Box("a")))

}