package compression

object Main extends App {

  val input = "aaawbbccaaaddddd"

  val (i, _, s) = input.tail.foldLeft((1, input.head, "" + input.head)){ case ((i, prev, s), current) =>
    if(prev == current) (i + 1, current, s) else if(i == 1)(1, current, s + current) else (1, current, s + i + current)
  }

  val result = if(i > 1) s + i else s
  println(result)




}
