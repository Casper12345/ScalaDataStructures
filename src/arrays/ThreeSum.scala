package arrays

object ThreeSum extends App {
  def threeSumNaive(sum: Int, input: List[Int]): Boolean = {
    input.toStream.flatMap { i1 =>
      input.toStream.flatMap { i2 =>
        input.toStream.map { i3 =>
          i1 + i2 + i3
        }
      }
    }.contains(sum)
  }


  println(threeSumNaive(20, List(1, 2, 3, 5, 6, 11, 15, 16, 17, 18)))
}
