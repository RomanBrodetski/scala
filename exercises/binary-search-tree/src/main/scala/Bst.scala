sealed trait Bst[+T] {
  def insert[U >: T](x: U)(implicit ordering: U => Ordered[U]): Bst[U]
  def toList: List[T]
}

case class Node[T](value: T, l: Bst[T], r: Bst[T]) extends Bst[T] {
  override def insert[U >: T](x: U)(implicit ordering: U => Ordered[U]): Bst[U] = {
    if (x > value) {
      Node(value, l, r.insert(x))
    } else {
      Node(value, l.insert(x), r)
    }
  }
  override def toList = l.toList ++ Seq(value) ++ r.toList
}

case object BNil extends Bst[Nothing] {
  override def insert[U](x: U)(implicit ordering: U => Ordered[U]): Bst[U] = {
    Node(x, BNil, BNil)
  }
  override def toList = Nil
}

object Bst {
  def apply[T](element: T)(implicit ordering: T => Ordered[T]) = BNil.insert(element)

  def fromList[T](l: List[T])(implicit ordering: T => Ordered[T]): Bst[T] =
    l.foldLeft[Bst[T]](BNil)((acc, el) => acc.insert(el))

  def toList[T](tree: Bst[T]): List[T] = tree.toList


  // as I've chosen an ADT structure for the tree (in this context, Option feels like null),
  // here is an implicit converstion to the format used in the tests (`LegacyBst`) - that way I don't have to amend them.
  implicit def bst2Legacy[T](x: Bst[T]): LegacyBst[T] = {
    def conv(f: Bst[T]): Option[LegacyBst[T]] = f match {
      case BNil => None
      case Node(v, l, r) => Some(LegacyBst(v, conv(l), conv(r)))
    }
    conv(x).get
  }
}

case class LegacyBst[T](value: T, left: Option[LegacyBst[T]], right: Option[LegacyBst[T]])
