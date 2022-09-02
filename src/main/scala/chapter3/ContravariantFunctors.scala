package chapter3

object ContravariantFunctors extends App {

  trait Contravariant [F[_]] {

    /*
      Makes sense for data types that represent
      transformations
    */
    def contramap [A, B] (fb: F[B]) (f: A => B): F[A]
  }

  trait Printable [A] { self =>

    def format (value: A): String

    /*
      We know how to format A, so if we know 
      the mapping B => A, we can also format B
    */
    def contramap [B] (f: B => A): Printable[B] = new Printable[B] {
      
      def format (b: B): String = self.format(f(b))
    }
  }

  object Printable {

    def apply [A] (implicit p: Printable[A]) = p

    implicit val stringPrintable: Printable[String] =
      identity(_)

    implicit val booleanPrintable: Printable[Boolean] =
      b => if (b) "yes" else "no"
  }

  final case class Box [A] (value: A)

  object Box {

    implicit def  printableBox [A: Printable]: Printable[Box[A]] =
      Printable[A].contramap[Box[A]](_.value)
  }
}