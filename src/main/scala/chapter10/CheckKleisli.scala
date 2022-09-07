package chapter10

import cats.data.NonEmptyList
import cats.data.Kleisli
import cats.syntax.either._
import cats.syntax.apply._

object CheckKleisli extends App {

  // Examples with Kleisli
  type Errors = NonEmptyList[String]

  type Result [A] = Either[Errors, A]

  type Check [A, B] = Kleisli[Result, A, B]

  // Create a check from a function
  def check [A, B] (f: A => Result[B]): Check[A, B] =
    Kleisli(f)

  // Create a check from a predicate
  def checkPredicate [A] (p: Predicate[Errors, A]): Check[A, A] =
    Kleisli(p.run)

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

    private val check = checkPredicate(
      longerThan(3) and alphanumeric
    )

    val make: Check[String, Username] =
      check.map(Username(_))
  }

  final case class Email (username: Username, domain: String)

  object Email {

    private val checkEmailDomain = checkPredicate(
      longerThan(2) and contains('.')
    )

    private val checkEmailSplit: Check[String, (String, String)] =
      check(_.split("@") match {
        case Array(l, r) => (l, r).asRight
        case _           => 
          error("Email must contain a username and a domain splitted by a single '@'")
            .asLeft
      })

    private val checkEmailParts: Check[(String, String), Email] =
      check {
        case (l, r) => 
          ( Username.make(l)
          , checkEmailDomain(r)
          ).mapN(Email.apply)
      } 

    val make: Check[String, Email] = 
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