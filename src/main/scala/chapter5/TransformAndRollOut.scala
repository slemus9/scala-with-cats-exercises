package chapter5

import cats.data.EitherT
import cats.syntax.applicative._
import cats.syntax.flatMap._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._

object TransformAndRollOut extends App {

  type Response [A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel (autobot: String) (implicit ec: ExecutionContext): Response[Int] = {

    def get = Future(
      powerLevels
        .get(autobot)
        .toRight(s"$autobot is unreachable")
    )

    EitherT(get)
  }

  def canSpecialMove (ally1: String, ally2: String) (implicit ec: ExecutionContext): Response[Boolean] =
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield p1 + p2 > 15


  def tacticalReport (ally1: String, ally2: String) (implicit ec: ExecutionContext): String = 
    Await.result(
      canSpecialMove(ally1, ally2).value, 2.seconds
    ) match {
      case Left(err)   => err
      case Right(able) => 
        if (able) s"$ally1 and $ally2 can perform special move"
        else s"$ally1 and $ally2 cannot perform special move"
    }

  import scala.concurrent.ExecutionContext.Implicits.global

  println(
    tacticalReport("Jazz", "Bumblebee")
  )

  println(
    tacticalReport("Bumblebee", "Hot Rod")
  )

  println(
    tacticalReport("Jazz", "Ironhide")
  )
}

