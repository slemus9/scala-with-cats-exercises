package chapter6

import cats.instances.list._
import cats.syntax.parallel._

object ParallelInstance extends App {

  // The parallel instance of list should behave as the zip function
  val xs = List(1, 2, 3)
  val ys = List(4, 5, 6)

  println(xs zip ys)
  println((xs, ys).parTupled)
}