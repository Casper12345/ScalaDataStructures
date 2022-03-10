package binary_search_tree

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Branch[A](l: Tree[A], r: Tree[A], v: A) extends Tree[A]
case object Leaf extends Tree[Nothing]

object Tree extends App {
  def fromList[A](xs: List[A], f:(A,A) => Boolean): Tree[A] = {
    def split(xs: List[A]): (A, List[A], List[A]) = {
      val (left, r) = xs.splitAt(xs.length / 2)
      val root = r.head
      val right = r.tail
      (root, left, right)
    }

    def go(xs: List[A]): Tree[A] = xs.length match {
      case 3 =>
        val (root, left, right) = split(xs)
        Branch(Branch(Leaf, Leaf, left.head), Branch(Leaf, Leaf, right.head), root)
      case 2 =>
        val (root, left, _) = split(xs)
        Branch(Branch(Leaf, Leaf, left.head), Leaf, root)
      case 1 =>
        val (root, _, _) = split(xs)
        Branch(Leaf, Leaf, root)
      case 0 =>
        Leaf
      case _ =>
        val (root, left, right) = split(xs)
        Branch(go(left), go(right), root)
    }

    go(xs.sortWith(f).distinct)
  }

  def insert[A](t: Tree[A], a: A, f: (A,A) => Boolean): Tree[A] = {
    def go(t: Tree[A]): Tree[A] = t match {
      case Branch(l: Branch[A], r: Branch[A], v) => if(f(a,v)) Branch(go(l), r, v) else Branch(l, go(r), v)
      case Branch(l: Branch[A], Leaf, v) => if(f(a,v)) Branch(go(l), Leaf, v) else Branch(l, Branch(Leaf, Leaf, a), v)
      case Branch(Leaf, r: Branch[A], v) => if(f(a,v)) Branch(Leaf, Branch(Leaf, Leaf, a), v) else Branch(Leaf, go(r), v)
      case Branch(Leaf, Leaf, v) => if(f(a,v)) Branch(Branch(Leaf, Leaf, a), Leaf, v) else Branch(Leaf, Branch(Leaf, Leaf, a), v)
      case Leaf => Leaf
    }
    go(t)

  }

  def contains[A](t: Tree[A], a: A, f: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(t: Tree[A]): Boolean = t match {
      case Branch(_, _, v) if v == a => true
      case Branch(l, r, v) if v != a => if(f(a,v)) go(l) else go(r)
      case Leaf => false
    }
    go(t)
  }

  val xs = List(1,2,3,4,5,6)
  val t = Tree.fromList[Int](xs, _ < _)
  println(t)
  val i = insert[Int](t, 6, _ <= _)
  println(i)
  println(contains[Int](i, 9, _ <= _))

}
