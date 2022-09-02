package chapter1

object Equality {

  import cats.Eq
  import cats.syntax.eq._
  import cats.instances.int._
  import cats.instances.string._

  final case class Cat (name: String, age: Int, color: String)

  object Cat {

    implicit val catEq = new Eq[Cat] {

      def eqv(c1: Cat, c2: Cat): Boolean = 
        c1.name === c2.name &&
        c1.age  === c2.age  &&
        c1.color === c2.color
    }
  }
}