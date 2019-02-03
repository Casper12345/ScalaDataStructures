package merge_sort

object MergeSort extends App {

  def mergeSort[A](f: (A, A) => Boolean) {

    def merge(l1: List[A], l2: List[A], acc: List[A], is: (Int, Int)): List[A] = {

      def iterate(xs: List[A], i: Int): (A, Int) = {
        (xs(i), i + 1)
      }

      val (a,b) = iterate(l1, is._1)
      val (c,d) = iterate(l2, is._2)

      if(f(a,c)){
        merge(l1, l2, acc :+ a, (b,d))
      } else {
        merge(l1, l2, acc :+ c, (b,d))
      }

      acc
    }


  }




}
