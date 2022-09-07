package chapter11

import scala.math

final case class GCounter (counters: Map[String, Int]) {

  def increment (machine: String, amount: Int): GCounter = {
    
    val newAmount = amount + this.counters.getOrElse(machine, 0)
    GCounter(this.counters + (machine -> newAmount))
  }

  def merge (that: GCounter): GCounter = {

    val common = this.counters.transform {
      case (k, cnt) => math.max(
        that.counters.getOrElse(k, cnt),
        cnt
      )
    }

    GCounter(
      that.counters ++ common
    )
  }

  def total: Int = counters.values.sum
}