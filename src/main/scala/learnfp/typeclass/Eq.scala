package learnfp.typeclass

trait Eq[A] {
  def eq(lhs: A, rhs: A): Boolean
}

object Eq {
  def eq[A: Eq](lhs: A, rhs: A): Boolean = implicitly[Eq[A]].eq(lhs, rhs)
}

class EqOps[A: Eq](lhs: A) {
  def ====(rhs: A): Boolean = Eq.eq(lhs, rhs)
}

object EqOps {
  implicit def toEqOps[A: Eq](lhs: A) = new EqOps(lhs)
}

object EqInstances {
  implicit val intEqInstance: Eq[Int] =
    (lhs: Int, rhs: Int) => lhs == rhs

  implicit val stringEqInstance: Eq[String] =
    (lhs: String, rhs: String) => lhs == rhs

  implicit def listEqInstance[A: Eq]: Eq[List[A]] =
    (lhs: List[A], rhs: List[A]) => lhs.length == rhs.length &&
      lhs.zip(rhs).forall {
        case (l, r) => implicitly[Eq[A]].eq(l, r)
      }
}
