package chapter4

import cats.data.Writer
import cats.instances.vector._  // Monoid[Vector]
import cats.syntax.applicative._ // pure
import cats.syntax.writer._  // tell

object WriterMonad extends App{

  // Writer[W, A] carries a log type W and a result type A
  // type Writer[W, A] = WriterT[Id, W, A]

  Writer(Vector(
  "It was the best of times",
  "it was the worst of times"
  ), 10)

  type Logged [A] = Writer[Vector[String], A]

  val emptyLogWithResult = 10.pure[Logged] // Writer((Vector(), 123))

  val logged = Vector("msg1", "msg2", "msg3").tell // Writer[Vector[String],Unit]

  val logWithResult = Writer(
    Vector("msg1", "msg2", "msg3"), 10
  )

  val logWithResult1 = 10.writer(
    Vector("msg1", "msg2", "msg3")
  )

  // val result = logWithResult.value
  // val log = logWithResult.written
  val (log, result) = logWithResult.run

  // flatMap appends the logs
  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("msg1", "msg2", "msg3").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1.run)

  val writer2 = writer1.mapWritten {
    _.map(_.toUpperCase)
  }

  println(writer2.run)

  val writer3 = writer1.bimap (
    log => log.map(_.toUpperCase),
    _ * 100
  )

  println(writer3.run)

  val writer4 = writer1.mapBoth { 
    (log, result) => (log.map(_ + "!"), result * 1000)
  }

  println(writer4.run)

  // Clear the log
  val writer5 = writer1.reset

  println(writer5.run)

  val writer6 = writer1.swap

  println(writer6.run)
}