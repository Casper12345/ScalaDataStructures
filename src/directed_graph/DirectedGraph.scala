import scala.annotation.tailrec


case class Node[A](edges: List[Node[A]], value: A)

object DirectedGraph extends App {


  val graph = Node(List(Node(List(Node(List(Node(List(Node(Nil, 7)), 6)), 4)), 2), Node(List(Node(Nil, 5)), 3)), 1)

  @tailrec
  def traverseDf[A](acc: List[A], stack: List[Node[A]]): List[A] = {
    def getUnvisited(xs: List[Node[A]]): Option[Node[A]] = xs match {
      case Nil => None
      case h :: t => if (!acc.contains(h.value)) Some(h) else getUnvisited(t)
    }

    stack match {
      case Nil => acc
      case h :: t => getUnvisited(h.edges) match {
        case None => traverseDf(acc, t)
        case Some(n) => traverseDf(acc :+ n.value, n :: h :: stack)
      }

    }
  }

  println(traverseDf(List(graph.value), graph :: Nil))

  @tailrec
  def traverseBf[A](acc: List[A], queue: List[Node[A]]): List[A] = {

    def getUnvisited(xs: List[Node[A]]): Option[Node[A]] = xs match {
      case Nil => None
      case h :: t => if (!acc.contains(h.value)) Some(h) else getUnvisited(t)
    }

    queue match {
      case Nil => acc
      case h :+ t => getUnvisited(t.edges) match {
        case None => traverseBf(acc, h)
        case Some(n) => traverseBf(acc :+ n.value, n :: queue)
      }

    }

  }

  println(traverseBf(List(graph.value), graph :: Nil))


}