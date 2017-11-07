object Test extends App {

  def another(i: Int*): Unit = {
    println(i)
  }



  def test(i: Int*): Unit = {

    another(i.tail: _*)

    val n = Seq[Int](1,2,3,4,5)


    println()

  }
   test(1,2,3,4,5)
}
