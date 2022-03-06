package merge_sort

object MergeSort extends App {

  def merge[A](a: List[A], b: List [A], f: (A,A) => Boolean): List[A] = {
    @scala.annotation.tailrec
    def go(a: List[A], b: List[A], result: List[A]): List[A] = {
      if(a.nonEmpty && b.nonEmpty){
        if(f(a.head, b.head)){
          go(a.tail , b, result :+ a.head)
        } else {
          go(a, b.tail, result :+ b.head)
        }
      } else {
        if (a.nonEmpty) {
          result ++ a
        } else if (b.nonEmpty) {
          result ++ b
        } else {
          result
        }
      }
    }
    go(a,b, Nil)
  }

  def mergeSort[A](l: List[A], f: (A,A) => Boolean): List[A] = {
    if(l.length == 1){
      l
    } else {
      val (a,b) = l.splitAt(l.length / 2)
      merge(mergeSort(a, f), mergeSort(b, f), f)
    }
  }

  println(mergeSort[Int](List(4,3,2,1,5,3,4,5,6), (a,b) => a < b))
  println(mergeSort[String](List("n","b","c","a","k"), (a,b) => a > b))

}
