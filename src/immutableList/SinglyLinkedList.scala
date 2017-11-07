package immutableList

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def head[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case Cons(h, _) => Some(h)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def printList[A](xs: List[A]): Unit = {
    print("List(")

    def helper(xs: List[A]): Unit = xs match {
      case Nil => print(")");
      case Cons(h, Nil) => print(h); helper(Nil)
      case Cons(h, t) => print(h + ", "); helper(t)
    }

    helper(xs)
  }

}


object Main extends App {
  val l: List[Int] = List.apply(1, 2, 3, 4, 5)
  val l2: List[Int] = List.apply()
  val head = List.head(l)
  val tail = List.tail(l)
  List.printList(l2)
  //println(tail)


}