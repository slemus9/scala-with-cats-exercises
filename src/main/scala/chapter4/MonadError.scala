package chapter4

import cats.Monad

/*
  F is the type of the monad
  E is the type of the error contained within F

  In cats, MonadError extends ApplicativeError
*/
trait MonadError [F[_], E] extends Monad[F] {

  def raiseError [A] (e: E): F[A]

  def handleErrorWith [A] (fa: F[A]) (f: E => F[A]): F[A]

  def handleError [A] (fa: F[A]) (f: E => A): F[A]

  /*
    Test an instance of F
    Fails if the predicate is not satisfied
  */
  def ensure [A] (fa: F[A]) (e: E) (p: A => Boolean): F[A]
}

object MonadErrorExample {

  import cats.MonadError
  import cats.instances.either._

  type ErrorOr [A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  val failure = monadError.raiseError("Badness")

  val handleFailureWith = monadError.handleErrorWith (failure) {
    case "Badness" => monadError.pure("It's Ok")
    case _         => monadError.raiseError("It's not Ok")
  }

  val handleFailure = monadError.handleError (failure) {
    case "Badness" => 42
    case _         => -1
  }

  // Ensure acts like a filter
  val filtered = 
    monadError.ensure (success) ("Number too low!") (_ > 1000)
  
  import cats.syntax.applicative._      // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._       // for ensure

  val success1 = 42.pure[ErrorOr]
  val failure1 = "Badness".raiseError[ErrorOr, Int]

  val handleFailureWith1 = failure1.handleErrorWith {
    case "Badness" => 256.pure
    case _         => "It's not Ok".raiseError
  }

  val filtered1 = success.ensure ("Number too low!") (_ > 1000)
}