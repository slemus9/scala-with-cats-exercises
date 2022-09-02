package chapter4

import cats.{Monad, Id}
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.option._

object IdentityMonad extends App {

  // type Id [A] = A

  def sumSquare [F[_]: Monad] (fa: F[Int], fb: F[Int]): F[Int] =
    for {
      a <- fa
      b <- fb
    } yield a*a + b*b

  println(
    sumSquare(3.some, 4.some)
  )

  println(
    sumSquare(
      List(1, 2, 3),
      List(4, 5)
    )
  )

  // Error
  // sumSquare(3, 4)
  println(
    sumSquare(3 : Id[Int], 4 : Id[Int])
  )

  println(
    sumSquare(Monad[Id].pure(3), Monad[Id].pure(4))
  )

  println(
    sumSquare(3.pure[Id], 4.pure[Id])
  )
}