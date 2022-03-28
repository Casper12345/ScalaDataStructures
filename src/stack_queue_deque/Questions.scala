package stack_queue_deque

import scala.annotation.tailrec


sealed trait Stack[+A]
case class Element[A](a: A, stack: Stack[A]) extends Stack[A]
case object End extends Stack[Nothing]

object Questions {

  def push[A](a: A, s: Stack[A]): Stack[A] = Element(a, s)

  def pop[A](s: Stack[A]): Option[(A, Stack[A])] = s match {
    case Element(a, stack) => Some((a, stack))
    case End => None
  }

  def top[A](s: Stack[A]): Option[A] = s match {
    case Element(a, _) => Some(a)
    case End => None
  }

  def size[A](s: Stack[A]): Int = {
    @tailrec
    def go(count: Int, s: Stack[A]): Int = s match {
      case Element(_, stack) => go(count + 1, stack)
      case End => count
    }
    go(0, s)
  }

  def isEmpty[A](s: Stack[A]): Boolean = s match {
    case Element(_, _) => false
    case End => true
  }

}
