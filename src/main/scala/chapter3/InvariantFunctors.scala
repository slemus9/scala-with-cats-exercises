package chapter3

object InvariantFunctors {

  
  trait Invariant [F[_]] {

    // Combination of map and contramap
    def imap [A, B] (fa: F[A]) (f: A => B) (g: B => A): F[B]
  }

  trait Codec [A] { self =>

    def encode (value: A): String
    def decode (value: String): A

    def imap [B] (dec: A => B, enc: B => A): Codec[B] = new Codec[B] {

      def encode (b: B): String = self.encode(enc(b))

      def decode (s: String): B = dec(self.decode(s))
    }
  }

  object Codec {

    def apply [A] (implicit codec: Codec[A]) = codec

    // Primitive instances
    implicit val stringCodec = new Codec[String] {
      
      def encode (s: String): String = s
      def decode (s: String): String = s
    }

    implicit val intCodec: Codec[Int] = stringCodec.imap(
      _.toInt, _.toString
    )

    implicit val boolCodec: Codec[Boolean] = stringCodec.imap(
      _.toBoolean, _.toString
    )

    implicit val doubleCodec: Codec[Double] = stringCodec.imap(
      _.toDouble, _.toString 
    )
  }

  final case class Box [A] (value: A)

  object Box {

    implicit def boxCodec [A: Codec] =
      Codec[A].imap[Box[A]](Box(_), _.value)
  }
}