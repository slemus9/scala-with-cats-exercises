package chapter6

import cats.{Applicative, Monad}
import cats.~>

/*
  Parallel allows us to take a type that has a Monad instance
  and convert it to some related type that has an Applicative 
  (or Semigroupal) instance instead. This new type will have 
  alternate semantics
*/
trait Parallel [M[_]] {

  type F[_] // Related type constructor that has an Applicative instance

  def applicative: Applicative[F]
  def monad: Monad[M]
  /*
    M ~> F is a function from a value with type
    M[A] to a value of type F[A]
  */
  def parallel: ~>[M, F]
}

object ParallelExamples extends App {

  import cats.Semigroupal
  import cats.instances.either._ 
  import cats.instances.vector._
  import cats.syntax.apply._ // for tupled and mapN
  import cats.syntax.parallel._ // for parTupled and parMapN

  // Example of an M ~> F instance
  object optionToList extends (Option ~> List) {

    def apply [A] (fa: Option[A]): List[A] = 
      fa.fold (List.empty[A]) (List(_))
  }

  // Examples of Parallel usage
  type ErrorOr [A] = Either[Vector[String], A]

  val error1: ErrorOr[Int] = Left(Vector("Error1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  val sequential = (error1, error1).tupled
  // Accumulates the error
  // Any type with a Semigroup instance will work
  val parallel   = (error1, error2).parTupled

  println(sequential)
  println(parallel)
}