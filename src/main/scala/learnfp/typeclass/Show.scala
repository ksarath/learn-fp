package learnfp.typeclass

trait Show[A] {
  def show(x: A): String
}

object Printer {
  def show[A: Show](x: A):String = implicitly[Show[A]].show(x)
}

object ShowInstances {
  implicit val intInstance: Show[Int] = _.toString

  implicit val doubleInstance: Show[Double] = _.toString

  implicit def listInstance[T: Show]: Show[List[T]] =
    (xs: List[T]) => "[" ++ xs.map(implicitly[Show[T]].show).mkString(", ") ++ "]"
}