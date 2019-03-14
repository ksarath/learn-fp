package learnfp.typeclass

trait TotalOrder[A] {
  def less(lhs: A, rhs: A): Boolean
}

object TotalOrderInstances {
  implicit val intInstance: TotalOrder[Int] = _ < _

  implicit val stringInstance: TotalOrder[String] = _ < _

  implicit def listInstance[T: TotalOrder]: TotalOrder[List[T]] =
    (lhs: List[T], rhs: List[T]) => lhs.zip(rhs).forall {
      case (l, r) => implicitly[TotalOrder[T]].less(l, r)
    }
}

object Comparator {
  @annotation.implicitNotFound("No instance of TotalOrder found")
  def less[A: TotalOrder](lhs: A, rhs: A): Boolean =
    implicitly[TotalOrder[A]].less(lhs, rhs)
}