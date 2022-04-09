package stack_queue_deque

import scala.annotation.tailrec

object Queue {
  sealed trait Queue[+A]
  case class Element[A](a: A, q: Queue[A]) extends Queue[A]
  case object End extends Queue[Nothing]

  def enqueue[A](a: A, q: Queue[A]): Queue[A] = Element(a, q)

  def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = {
    var optA: Option[A] = None
    def go(q: Queue[A]): Queue[A] = q match {
      case End => End
      case Element(a, End) => optA = Some(a); End
      case Element(a, Element(e, End)) => optA = Some(e); Element(a, End)
      case Element(a, q) => Element(a, go(q))
    }
    val res = go(q)
    optA.map(a => (a, res))

  }

}

object Stack {
  sealed trait Stack[+A]
  case class Element[A](a: A, stack: Stack[A]) extends Stack[A]
  case object End extends Stack[Nothing]

  def push[A](a: A, s: Stack.Stack[A]): Stack.Stack[A] = Stack.Element(a, s)

  def pop[A](s: Stack.Stack[A]): Option[(A, Stack.Stack[A])] = s match {
    case Stack.Element(a, stack) => Some((a, stack))
    case Stack.End => None
  }

  def top[A](s: Stack.Stack[A]): Option[A] = s match {
    case Stack.Element(a, _) => Some(a)
    case Stack.End => None
  }

  def size[A](s: Stack.Stack[A]): Int = {
    @tailrec
    def go(count: Int, s: Stack.Stack[A]): Int = s match {
      case Stack.Element(_, stack) => go(count + 1, stack)
      case Stack.End => count
    }
    go(0, s)
  }

  def isEmpty[A](s: Stack.Stack[A]): Boolean = s match {
    case Stack.Element(_, _) => false
    case Stack.End => true
  }
}
