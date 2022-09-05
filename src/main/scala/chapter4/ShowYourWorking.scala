package chapter4

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

import cats.data.Writer
import cats.syntax.writer._

object ShowYourWorking extends App {

  def slowly [A] (body: => A) = 
    try body finally Thread.sleep(100)

  def factorial (n: Int): Int = {

    val ans = slowly {
      if (n == 0) 1 else n * factorial(n - 1)
    }
    println(s"fact $n $ans")
    ans
  }

  // Await.result(
  //   Future.sequence(
  //     Vector(Future(factorial(5)), Future(factorial(5)))
  //   ),
  //   5.seconds
  // )

  type Logger [A] = Writer[Vector[String], A]

  def factorialWithLogs (n: Int): Logger[Int] = {
    
    def go (acc: Logger[Int], i: Int): Logger[Int] =
      if (n == i) acc
      else {
        val next = i + 1
        val newAcc = acc.flatMap { x => 
          val res = x * next
          res.writer(Vector(s"fact $next $res")) 
        }

        go(newAcc, next)
      }

    val fact0 = 1.writer(Vector("fact 0 1"))
    go(fact0, 0)
  }

  val parallelRes = Await.result(
    Future.sequence(
      Vector(Future(factorialWithLogs(10)), Future(factorialWithLogs(5)))
    ),
    5.seconds
  )

  val Vector(res1, res2) = parallelRes

  println(res1.run)
  println(res2.run)
}
