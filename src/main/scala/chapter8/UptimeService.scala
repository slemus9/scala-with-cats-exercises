package chapter8

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import cats.Applicative
import cats.syntax.functor._
import cats.syntax.traverse._
// import cats.instances.future._

class UptimeService [F[_]: Applicative] (client: UptimeClient[F]) {

  def getTotalUptime (hostnames: List[String]) (implicit ec: ExecutionContext): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}