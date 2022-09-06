package chapter6

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

object TheProductOfLists {

  def listProduct [A, B] (xs: List[A], ys: List[B]): List[(A, B)] =
    for {
      x <- xs
      y <- ys
    } yield (x, y)

  def product [F[_]: Monad, A, B] (fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)
}