package chapter7

import cats.Applicative
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.instances.vector._

object TraversingWithVectors extends App {

  def listTraverse [F[_]: Applicative, A, B] (xs: List[A]) (f: A => F[B]): F[List[B]] =
    xs.foldRight(List.empty[B].pure[F]) {
      (x, next) => (f(x), next).mapN(_ :: _)
    }

  def listSequence [F[_]: Applicative, A] (xs: List[F[A]]): F[List[A]] =
    listTraverse (xs) (identity)


  /*
    Should yield: 
      Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    
    Since Vector is a Monad, its Semigroupal instance is implemented
    via flatMap, and the `product`function yields the cartesian product
    of the elements in the vector
  */ 
  println(
    listSequence(
      List(Vector(1, 2), Vector(3, 4))
    )
  )

  /*
    Should yield:
      Vector(
        List(1, 3, 5), List(1, 3, 6), 
        List(1, 4, 5), List(1, 4, 6),
        List(2, 3, 5), List(2, 3, 6),
        List(2, 4, 5), List(2, 4, 6)
      )
  */
  println(
    listSequence(
      List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
    )
  )
}