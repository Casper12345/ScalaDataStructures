package singly_linked_list

object Questions extends App {


  // Remove Dups: Write code to remove duplicates from an unsorted linked list. FOLLOW UP
  // How would you solve this problem if a temporary buffer is not allowed? Hints: #9, #40

  def removeDups[A](xs: LinkedList[A]): LinkedList[A] = {

    def go(xs: LinkedList[A], l: List[A]): LinkedList[A] = xs match {
      case Cons(h, t) =>
        if(l.contains(h)) go(t, l) else Cons(h, go(t, h :: l))
      case Nil => Nil
    }
    go(xs, List.empty)
  }

  // Return Kth to Last: Implement an algorithm to find the kth to last element of a singly linked list.

  def last[A](xs: LinkedList[A], k: Int): LinkedList[A] = {
    def go(i: Int, xs: LinkedList[A]): LinkedList[A] = xs match {
      case Cons(h, t) => if(i < k) go(i + 1, t) else Cons(h, go(i, t))
      case Nil => Nil
    }
    go(0, xs)
  }

    println(last(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))), 2))

}
