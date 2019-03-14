package learnfp.typeclass

trait TypeClass[A] {
  def foo(x: A): String
}

object TypeClassInstances {
  implicit val intInstance: TypeClass[Int] = _ => "int"

  implicit val stringInstance: TypeClass[String] = _ => "string"
}

object TypeClassUser {
  def foo[A: TypeClass](x: A): String =
    implicitly[TypeClass[A]].foo(x)
}
