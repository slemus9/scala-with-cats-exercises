package chapter10

import cats.data.Validated
import cats.data.Validated._
import cats.Semigroup

sealed trait Check [E, A, B] { self => 

  def apply (a: A) (implicit E: Semigroup[E]): Validated[E, B]

  def map [C] (f: B => C): Check[E, A, C] = 
    Check.Map(self, f)

  def flatMap [C] (f: B => Check[E, A, C]) =
    Check.FlatMap(self, f)

  def andThen [C] (that: Check[E, B, C]) =
    Check.AndThen(self, that)
}

object Check {

  def apply [E, A, B] (f: A => Validated[E, B]) =
    Single(f)

  def apply [E, A] (p: Predicate[E, A]) = FromPredicate(p)

  final case class FromPredicate [E, A] (p: Predicate[E, A]) extends Check[E, A, A] {

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, A] = 
      p(a)
  }

  final case class Single [E, A, B] (f: A => Validated[E, B]) extends Check[E, A, B] {

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, B] = 
      f(a)
  }

  final case class Map [E, A, B, C] (check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
  
    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, C] = 
      check(a).map(f)
  } 

  final case class FlatMap [E, A, B, C] (
    check: Check[E, A, B], 
    f: B => Check[E, A, C]
  ) extends Check[E, A, C] { 

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E,C] = 
      check(a).andThen(f (_) (a))
  }

  final case class AndThen [E, A, B, C] (
    check1: Check[E, A, B],
    check2: Check[E, B, C]
  ) extends Check [E, A, C] {

    def apply (a: A) (implicit E: Semigroup[E]): Validated[E, C] = 
      check1(a).andThen(check2(_))
  } 
}

object CheckExamples extends App {

  import cats.data.NonEmptyList
  import cats.syntax.validated._
  import cats.syntax.apply._

  type Errors = NonEmptyList[String]

  // Primitives for Predicate
  def error (s: String): Errors = 
    NonEmptyList(s, Nil)

  def longerThan (n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      s => s.size > n
    )

  def alphanumeric: Predicate[Errors, String] = 
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      s => s.forall(_.isLetterOrDigit)
    )

  def contains (c: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $c"),
      s => s.contains(c)
    )

  def containsOnce (c: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $c only once"),
      s => s.filter(_ == c).size == 1
    )

  // Model
  final case class Username (value: String)

  object Username {

    private val check = Check(
      longerThan(3) and alphanumeric
    )

    val make: Check[Errors, String, Username] =
      check.map(Username(_))
  }

  final case class Email (username: Username, domain: String)

  object Email {

    private val checkEmailDomain = Check(
      longerThan(2) and contains('.')
    )

    private val checkEmailSplit: Check[Errors, String, (String, String)] =
      Check(_.split("@") match {
        case Array(l, r) => (l, r).validNel
        case _           => 
          "Email must contain a username and a domain splitted by a single '@'"
            .invalidNel
      })

    private val checkEmailParts: Check[Errors, (String, String), Email] =
      Check {
        case (l, r) => 
          ( Username.make(l)
          , checkEmailDomain(r)
          ).mapN(Email.apply)
      }

    val make: Check[Errors, String, Email] = 
      checkEmailSplit andThen checkEmailParts
  }

  // Examples
  println(
    Username.make("user1234")
  )

  println(
    Username.make("%&/")
  )

  println(
    Email.make("user1@example.com")
  )

  println(
    Email.make("%&/@example.com")
  )

  println(
    Email.make("%&/@com")
  )
}