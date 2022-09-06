package chapter7

import cats.instances.option._
import cats.syntax.traverse._

object TraversingWithOptions extends App {

  def process (inputs: List[Int]) =
    inputs.traverse {
      n => if (n % 2 == 0) Some(n) else None
    }

  // Should yield: Some(List(2, 4, 6))
  println(
    process(List(2, 4, 6)),
  )

  // Should yield: None
  // Option is a Monad, so Semigroupal is implemented with flatMap, thus its fail-fast behaviour
  println(
    process(List(1, 2, 3))
  )
}