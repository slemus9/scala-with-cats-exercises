package chapter11

import cats.Monoid
import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._

/*
  increment requires a monoid
  total     requires a commutative monoid
  merge     requires an idempotent commutative monoid (bounded semilattice)
*/
final case class GGCounter [A] (counters: Map[String, A]) {

  def increment (machine: String, amount: A) (implicit A: Monoid[A]): GGCounter[A] = {
    
    val newAmount = amount |+| this.counters.getOrElse(
      machine, A.empty
    )
    GGCounter(this.counters + (machine -> newAmount))
  }

  def merge (that: GGCounter[A]) (implicit A: BoundedSemiLattice[A]): GGCounter[A] = 
    GGCounter(this.counters |+| that.counters)

  def total (implicit A: CommutativeMonoid[A]): A = A.combineAll(
    this.counters.values
  )
}