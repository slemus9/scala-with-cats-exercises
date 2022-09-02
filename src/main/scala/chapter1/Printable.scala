package chapter1

trait Printable [A] {

  def format (value: A): String
}

object Printable {

  def apply [A] (implicit p: Printable[A]) = p

  def format [A] (value: A) (implicit p: Printable[A]): String =
    p.format(value)

  def print [A] (value: A) (implicit p: Printable[A]): Unit =
    println(p.format(value))
}

object PrintableInstances {

  implicit def printableString: Printable[String] =
    identity(_)

  implicit def printableInt: Printable[Int] =
    _.toString
}

object PrintableSyntax {

  implicit class PrintableOps [A: Printable] (value: A) {

    def format = Printable[A].format(value)

    def print = Printable.print(value)
  }
}

final case class Cat (name: String, age: Int, color: String)

object Cat {

  import PrintableInstances._
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val printableCat = new Printable[Cat] {

    def format(cat: Cat): String = {

      val nameStr = Printable.format(cat.name)
      val ageStr = Printable.format(cat.age)
      val colorStr = Printable.format(cat.color)

      s"$nameStr is a $ageStr year-old $colorStr cat."
    }
  }

  implicit val showCat: Show[Cat] = Show.show {
    case Cat(name, age, color) =>
      s"${name.show} is a ${age.show} year-old ${color.show} cat."
  }
}