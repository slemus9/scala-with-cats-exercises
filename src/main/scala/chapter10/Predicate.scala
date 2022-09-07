package chapter10

import cats.data.Validated
import cats.data.Validated._
import cats.Semigroup
import cats.syntax.semigroup._
import cats.syntax.validated._
import cats.syntax.apply._

/*
  For a predicate p of type Predicate[E, A] 
  and elements a1 and a2 of type A, if p(a1) == Success(a2) 
  then a1 == a2
*/
sealed trait Predicate [E, A] { self =>

  def and (that: Predicate[E, A]): Predicate[E, A] = 
    Predicate.And(self, that)

  def or (that: Predicate[E, A]): Predicate[E, A] =
    Predicate.Or(self, that)

  def apply (a: A) (implicit E: Semigroup[E]): Validated[E, A] 

  def run (implicit E: Semigroup[E]): A => Either[E, A] = 
    self(_).toEither
}

object Predicate {

  def apply [E, A] (f: A => Validated[E, A]): Predicate[E, A] = Single(f)

  def lift [E, A] (e: E, p: A => Boolean): Predicate[E, A] = Predicate {
    a => if (p(a)) a.valid else e.invalid
  }

  final case class Single [E, A] (f: A => Validated[E, A]) extends Predicate[E, A] {

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, A] = 
      f(a)
  }

  final case class And [E, A] (predicate1: Predicate[E, A], predicate2: Predicate[E, A]) extends Predicate[E, A] {

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, A] = 
      (predicate1(a), predicate2(a)).mapN(
        (_, _) => a
      )
  }

  final case class Or [E, A] (predicate1: Predicate[E, A], predicate2: Predicate[E, A]) extends Predicate[E, A] {

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, A] = 
      predicate1(a) match {
        case Valid(_)      => a.valid
        case Invalid(e1)   => predicate2(a) match {
          case Invalid(e2) => (e1 |+| e2).invalid
          case Valid(r)    => a.valid
        }
      }
  }
} 