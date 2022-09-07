package chapter11

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._

trait GCounterF [F[_, _], K, V] {

  def increment (f: F[K, V]) (key: K, value: V)
    (implicit V: CommutativeMonoid[V]): F[K, V]

  def merge (f1: F[K, V], f2: F[K, V])
    (implicit V: BoundedSemiLattice[V]): F[K, V]

  def total (f: F[K, V])
    (implicit V: CommutativeMonoid[V]): V
}

object GCounterF {

  import KeyValueStore._

  def apply [F[_, _], K, V] 
    (implicit 
      counter: GCounterF[F, K, V]
    ) = counter

  // implicit def mapGC [K, V] = new GCounterF[Map, K, V] {

  //   def increment (f: Map[K, V]) (key: K, value: V) 
  //     (implicit V: CommutativeMonoid[V]): Map[K, V] = {

  //     val newValue = value |+| f.getOrElse(key, V.empty)
  //     f + (key -> newValue)
  //   }

  //   def merge (f1: Map[K,V], f2: Map[K,V]) 
  //     (implicit V: BoundedSemiLattice[V]): Map[K,V] = 
  //     f1 |+| f2

  //   def total (f: Map[K, V]) 
  //     (implicit V: CommutativeMonoid[V]): V = 
  //     V.combineAll(f.values)
  // }

  implicit def gcounterInstance [F[_, _], K, V] (
    implicit 
      KVS: KeyValueStore[F]
    , F: CommutativeMonoid[F[K, V]]
  ) = new GCounterF[F, K, V] {

    def increment (f: F[K,V]) (key: K, value: V) 
      (implicit V: CommutativeMonoid[V]): F[K,V] = {

      val newValue = value |+| f.getOrElse(key, V.empty)
      f.put(key, newValue)
    }
      

    def merge (f1: F[K,V], f2: F[K,V]) 
      (implicit V: BoundedSemiLattice[V]): F[K,V] = 
      f1 |+| f2

    def total (f: F[K,V]) 
      (implicit V: CommutativeMonoid[V]): V = 
      V.combineAll(f.values)
  }

  implicit def mapGC [K, V: BoundedSemiLattice] = 
    gcounterInstance[Map, K, V]
}

object GCounterExample extends App {

  import cats.instances.int._

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounterF[Map, String, Int]

  val merged = counter.merge(g1, g2)

  val total = counter.total(merged)

  println(merged)
  println(total)
}