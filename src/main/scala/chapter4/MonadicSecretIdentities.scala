package chapter4

import cats.Monad

object MonadicSecretIdentities {

  type Id [A] = A

  object Id {

    implicit val idMonad = new Monad[Id] {

      def pure [A] (x: A): Id[A] = x

      override def map [A, B] (fa: Id[A]) (f: A => B): Id[B] = 
        f(fa)

      // flatMap is the same as map for Id
      def flatMap [A, B] (fa: Id[A]) (f: A => Id[B]): Id[B] = 
        f(fa)

      def tailRecM [A, B] (a: A) (f: A => Id[Either[A,B]]): Id[B] = f(a) match {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => b
      }
    }
  }
}