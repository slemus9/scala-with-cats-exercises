package chapter8

import scala.concurrent.Future
import cats.Id

class TestUptimeClientImpl (hosts: Map[String, Int]) extends TestUptimeClient {

  def getUptime (hostname: String): Id[Int] = 
    hosts.getOrElse(hostname, 0)
}

object TestExample {

  import scala.concurrent.ExecutionContext.Implicits.global

  val host = Map("host1" -> 10, "host2" -> 6)
  val client   = new TestUptimeClientImpl(host)
  val service  = new UptimeService(client)
  val actual   = service.getTotalUptime(host.keys.toList)
  val expected = host.values.sum

  assert(actual == expected)
}