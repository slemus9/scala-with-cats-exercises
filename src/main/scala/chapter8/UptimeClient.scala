package chapter8

import scala.concurrent.Future
import cats.Id

/*
  trait UptimeClient {

    def getUptime (hostname: String): Future[Int]
  }

  For testing, we need to implement an asynchronous version (for production)
  and a synchronous version (for testing) of the UptimeClient

  trait RealUptimeClient extends UptimeClient {
    def getUptime(hostname: String): Future[Int]
  }

  trait TestUptimeClient extends UptimeClient {
    def getUptime(hostname: String): Int
  }

  What should be the return type of the getUptime method?

  trait UptimeClient {
    def getUptime(hostname: String): ???
  }
*/
trait UptimeClient [F[_]] {

  def getUptime (hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {

  // just to verfy if it compiles
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient extends UptimeClient[Id] {

  // just to verfy if it compiles
  def getUptime(hostname: String): Id[Int]
}