package chapter7

import cats.Applicative

trait Traverse [F[_]] {

  def traverse [G[_]: Applicative, A, B]
    (inputs: F[A]) (f: A => G[B]): G[F[B]]

  def sequence [G[_]: Applicative, B]
    (inputs: F[G[B]]): G[F[B]] = traverse (inputs) (identity)
}