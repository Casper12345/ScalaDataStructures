package binarySearch

case class Box[A](id: A) extends Ordered[Box[A]] {
  override def compare(that: Box[A]): Int = {
    if (id.hashCode() > that.id.hashCode()) {
      1
    } else if (id.hashCode() < that.id.hashCode()) {
      -1
    } else {
      0
    }
  }
}
