package chapter6

import cats.{Semigroupal, Functor}

trait Apply [F[_]] extends Semigroupal[F] with Functor[F] {

  def ap [A, B] (ff: F[A => B]) (fa: F[A]): F[B]

  def product [A, B] (fa: F[A], fb: F[B]): F[(A, B)] = {

    val zipWithA = map (fa) { a => (b: B) => (a, b) }
    ap (zipWithA) (fb)
  }
}

/*
  We could say that Applicative is related to Apply 
  as Monoid is related to Semigroup
*/
trait Applicative [F[_]] extends Apply[F] {

  def pure [A] (a: A): F[A]
}