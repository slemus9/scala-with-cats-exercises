package chapter7

import cats.data.Validated
import cats.instances.list._ // for Monoid
import cats.syntax.traverse._

object TraversingWithValidated extends App {

  type ErrorsOr [A] = Validated[List[String], A]

  def process (inputs: List[Int]): ErrorsOr[List[Int]] =
    inputs.traverse { n => 
      if (n % 2 == 0) Validated.valid(n)
      else Validated.invalid(List(s"$n is not even"))
    }

  println(
    process(List(2, 4, 6))
  )

  // Semigroupal instance for validated accumulates errors
  println(
    process(List(1, 2, 3))
  )
}