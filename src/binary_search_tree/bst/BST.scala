package binary_search_tree.bst

import binary_search_tree.bst.BST.{End, Node}

import scala.collection.mutable.ListBuffer

sealed trait Tree[+K,+V]
object BST {
  case class Node[K,V](key: K, value: V, left: Tree[K,V], right: Tree[K,V], size: Int) extends Tree[K,V]
  case object End extends Tree[Nothing, Nothing]
}

class BST[K,V](f: (K,K) => Int) {

  def insert(key: K, value: V, t: Tree[K, V]): Tree[K, V] = t match {
    case End => Node(key, value, End, End, 0)
    case n@Node(k, _, l, r, size) =>
      if (f(key, k) == -1) n.copy(left = insert(key, value, l), size = size + 1)
      else if (f(key, k) == 1) n.copy(right = insert(key, value, r), size = size + 1)
      else n.copy(value = value)
  }

  def get(key: K, t: Tree[K,V]): Option[V] = t match {
    case End => None
    case Node(k, v, left, right, _) =>
      if(f(key, k) == -1) get(key, left) else if(f(key, k) == 1) get(key, right) else Some(v)
  }

  def min(t: Tree[K,V]): Option[(K,V)] = t match {
    case End => None
    case Node(key, value, End, _, _) => Some((key, value))
    case Node(_, _, l, _, _) => min(l)
  }

  def max(t: Tree[K,V]): Option[(K,V)] = t match {
    case End => None
    case Node(key, value, _, End, _) => Some((key, value))
    case Node(_, _, _, r, _) => min(r)
  }

  def floor(k: K, t: Tree[K,V]): Option[(K,V)] = t match {
    case End => None
    case Node(key, value, left, right, _) =>
      if(f(k,key) == 0) Some(key, value)
      else if(f(k,key) == -1) floor(k, left)
      else {
        floor(k, right) match {
          case s: Some[(K,V)] => s
          case None => Some(key, value)
        }
      }
  }

  def size(t: Tree[K,V]): Int = t match {
    case End => 0
    case Node(_, _, left, right, _) =>
      1 + size(left) + size(right)
  }

  def rank(k: K, t: Tree[K,V]): Int = t match {
    case End => 0
    case Node(key, _, left, right, _) =>
      if(f(k,key) < 0) rank(k, left)
      else if (f(k,key) > 0) 1 + size(left) + rank(key, right)
      else size(left)
  }

  def inorder(t: Tree[K,V]): List[(K,V)] = {
    val lb = ListBuffer.empty[(K,V)]

    def go(t: Tree[K,V]): Unit = t match {
      case End => ()
      case Node(key, value, left, right, _) =>
        go(left)
        lb += ((key, value))
        go(right)
    }
    go(t)
    lb.toList
  }

  def preOrder(t: Tree[K,V]): List[(K,V)] = {
    val lb = ListBuffer.empty[(K,V)]

    def go(t: Tree[K,V]): Unit = t match {
      case End => ()
      case Node(key, value, left, right, _) =>
        lb += ((key, value))
        go(left)
        go(right)
    }
    go(t)
    lb.toList
  }

  def postOrder(t: Tree[K,V]): List[(K,V)] = {
    val lb = ListBuffer.empty[(K,V)]

    def go(t: Tree[K,V]): Unit = t match {
      case End => ()
      case Node(key, value, left, right, _) =>
        go(left)
        go(right)
        lb += ((key, value))
    }
    go(t)
    lb.toList
  }


  def deleteMinimum(t: Tree[K,V]): Tree[K,V] = t match {
    case n@Node(_, _, Node(_, _, End, r, _), _, size) =>
      n.copy(left = r, size = size - 1)
    case n@Node(_, _, left, _, size) =>
      n.copy(left = deleteMinimum(left), size = size - 1)
    case End => End
  }
}

object Main extends App {
  def func(k1: Int, k2: Int): Int = if(k1 < k2) -1 else if (k1 > k2) 1 else 0
  val t = new BST[Int, String](func)

  val t1 = t.insert(4, "hey", End)
  val t2 = t.insert(2, "yes", t1)
  val t3 = t.insert(6, "no", t2)
  val t4 = t.insert(8, "ok", t3)
  val t5 = t.insert(3, "hell yes", t4)
  val t6 = t.insert(1, "mmyes", t5)

  println(t1)
  println(t2)
  println(t3)
  println(t4)
  println(t5)
//  println(t6)
  println(t.deleteMinimum(t5))

}


