package bubbleSort

import binarySearch.Box

object BubbleSort {
  def apply() = new BubbleSort
}

class BubbleSort {

  def sort[A <: Ordered[A]](lst: List[A]): List[A] = {

    def helper(lst: List[A]): List[A] = lst match {
      case Nil => Nil
      case h :: Nil => h :: Nil
      case h :: t =>
        if (h > t.head) {
          t.head :: sort(h :: t.drop(1))
        } else {
          h :: sort(t)
        }
    }

    def counter(lst: List[A], i: Int): List[A] = i match {
      case 0 => lst
      case _ => counter(helper(lst), i - 1)
    }

    counter(helper(lst), lst.size)

  }


}

object Main extends App {
  val b = BubbleSort.apply()
  val l = b.sort(List[Box[String]](Box("8"), Box("7"), Box("4"), Box("3"), Box("5"), Box("10")))
  println(l)

}