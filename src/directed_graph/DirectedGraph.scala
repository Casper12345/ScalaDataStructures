import scala.annotation.tailrec
import scala.collection.mutable


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

  @tailrec
  def traverseBfWeighted[A](acc: List[WeightedNode[A]], queue: List[WeightedNode[A]], dist: WeightedNode[A]): List[WeightedNode[A]] = {

    def getUnvisited(xs: List[WeightedNode[A]]): Option[WeightedNode[A]] = xs match {
      case Nil => None
      case h :: t => if (!acc.contains(h.value)) Some(h) else getUnvisited(t)
    }

    queue match {
      case Nil => acc
      case h :+ t => getUnvisited(t.edges) match {
        case None => traverseBfWeighted(acc, h, dist)
        case Some(n) => if(dist == n) traverseBfWeighted(acc :+ n.copy(weight = 0), n :: queue, dist) else traverseBfWeighted(acc :+ n.copy(weight = Int.MaxValue), n :: queue, dist)
      }

    }

  }



  case class WeightedNode[A](edges: List[WeightedNode[A]], value: A, weight: Int = Int.MaxValue)

//  def dijkstra[A](n: Node[A], dist: Node[A]): (List[Node[A]], List[Node[A]]) = {
//    val previousNodes: mutable.Map[Node[A], Int] = mutable.Map()
//
//
//
//
//
//
//  }



}