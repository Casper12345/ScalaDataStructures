package merge_sort

object MergeSort extends App {

  def mergeSort[A](f: (A, A) => Boolean) {

    def merge(l1: List[A], l2: List[A], acc: List[A], is: (Option[Int], Option[Int])): List[A] = {

      def iterate(xs: List[A], i: Int): Option[(A, Int)] = {
        if (i >= xs.size){
          None
        } else {
          Some((xs(i), i + 1))
        }
      }

      is._1.flatMap { itt1 =>
        is._2.map { itt2 =>
          iterate(l1, itt1) match {
            case None =>
              iterate(l2, itt2) match {
                case None => acc
                case Some((a2, i2)) =>
                  merge(l1, l2, acc :+ a2,  (None, Some(i2)))

              }
            case Some((a1, i1)) =>
              iterate(l2, itt2) match {
                case None => acc
                case Some((a2, i2)) =>
                  if(f(a2, a1)){
                    merge(l1, l2, acc :+ a1,  (Some(i1), Some(i2)))
                  } else {
                    merge(l1, l2, acc :+ a2,  (Some(i1), Some(i2)))
                  }
              }
          }
        }
      }.getOrElse(acc)

    }

  }




}
