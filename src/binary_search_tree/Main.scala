package binary_search_tree

sealed trait Tree
case class Branch[A](l: Tree, r: Tree, v: A) extends Tree
case object Leaf extends Tree

object Tree extends App {
  def fromList[A](xs: List[A], f:(A,A) => Boolean): Tree = {
    def split(xs: List[A]): (A, List[A], List[A]) = {
      val (left, r) = xs.splitAt(xs.length / 2)
      val root = r.head
      val right = r.tail
      (root, left, right)
    }

    def go(xs: List[A]): Tree = xs.length match {
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
      val xs = List("b","a","c","e","d")
      println(Tree.fromList[String](xs, _ < _))

}
