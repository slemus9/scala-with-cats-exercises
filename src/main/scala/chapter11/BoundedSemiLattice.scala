package chapter11

import cats.kernel.CommutativeMonoid

// An Idempotent commutative monoid
trait BoundedSemiLattice [A] extends CommutativeMonoid[A] {

  def combine (x: A, y: A): A

  def empty: A
}

object BoundedSemiLattice {

  def apply [A] (implicit bsl: BoundedSemiLattice[A]) = bsl

  implicit val positiveIntBSL = new BoundedSemiLattice[Int] {

    def combine (x: Int, y: Int): Int = 
      scala.math.max(x, y)

    def empty: Int = 0
  }

  implicit def setBSL [A] = new BoundedSemiLattice[Set[A]] {

    def combine (x: Set[A], y: Set[A]): Set[A] = 
      x union y

    def empty: Set[A] = Set.empty
  }

  implicit def mapBSL [K, V] (implicit V: BoundedSemiLattice[V]) = new BoundedSemiLattice[Map[K, V]] {
    
    def combine (m1: Map[K, V], m2: Map[K, V]): Map[K, V] = 
      m2 ++ m1.transform {
        case (k, v) => V.combine(
          m2.getOrElse(k, V.empty),
          v
        )
      }

    def empty: Map[K, V] = Map.empty
  }
}