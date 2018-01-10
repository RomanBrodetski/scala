trait SimpleLinkedList[+T] {
  def isEmpty: Boolean
  def value: T
  def add[Q >: T](item: Q): SimpleLinkedList[Q]
  def next: SimpleLinkedList[T]
  def toSeq: Seq[T]
  def reverse: SimpleLinkedList[T] = {
    @annotation.tailrec
    def reverseRec(list: SimpleLinkedList[T], acc: SimpleLinkedList[T]): SimpleLinkedList[T] = {
      list match {
        case Null => acc
        case Cons(v, next)  => reverseRec(next, Cons(v, acc))
      }
    }
    reverseRec(this, Null)
  }
}

case object Null extends SimpleLinkedList[Nothing] {
  override val isEmpty = true
  override def value = throw new NoSuchElementException()
  override def next = throw new NoSuchElementException()
  override def add[Q](item: Q): Cons[Q] = Cons(item, Null)
  override def toSeq = Nil
}

case class Cons[T](override val value: T, override val next: SimpleLinkedList[T]) extends SimpleLinkedList[T] {
  override val isEmpty = false
  override def add[Q >: T](item: Q): Cons[Q] = Cons(value, next.add(item))
  override val toSeq = value +: next.toSeq
}

object SimpleLinkedList {
  def apply[T](): SimpleLinkedList[T] = Null
  def apply[T](ts: T*): SimpleLinkedList[T] = fromSeq(ts)
  def fromSeq[T](seq: Seq[T]): SimpleLinkedList[T] =
    seq.foldLeft(SimpleLinkedList[T]())((acc, t) => acc.add(t))
}
