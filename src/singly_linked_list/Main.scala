package singly_linked_list

import scala.annotation.tailrec

object Main extends App {

}


sealed trait LinkedList[+A]
case class Cons[A](h: A, t: LinkedList[A]) extends LinkedList[A]
case object Nil extends LinkedList[Nothing]

object LinkedList {

  def size[A](xs: LinkedList[A]): Int = {
    @tailrec
    def go(count: Int, xs: LinkedList[A]): Int = xs match {
      case Nil => count
      case Cons(_, t) => go(count + 1, t)
    }
    go(0, xs)
  }

  def isEmpty[A](xs: LinkedList[A]): Boolean =
    size(xs) == 0

  def firstOpt[A](xs: LinkedList[A]): Option[A] = xs match {
    case Cons(h, _) => Some(h)
    case Nil => None
  }

  def last[A](xs: LinkedList[A]): Option[A] = {
    @tailrec
    def go(xs: LinkedList[A]): Option[A] = xs match {
      case Cons(h, Nil) => Some(h)
      case Cons(_, t) => go(t)
      case Nil => None
    }

    go(xs)
  }

  def addFirst[A](xs: LinkedList[A], a: A): LinkedList[A] = Cons(a, xs)

  def addLast[A](xs: LinkedList[A], a: A): LinkedList[A] = {
    def go(xs: LinkedList[A]): LinkedList[A] = xs match {
      case Cons(h, Nil) => Cons(h, Cons(a, Nil))
      case Cons(h, t) => Cons(h, go(t))
      case Nil => Nil
    }
    go(xs)
  }

  def removeFirst[A](xs: LinkedList[A]): LinkedList[A] = xs match {
    case Cons(_, t) => t
    case Nil => Nil
  }

}

