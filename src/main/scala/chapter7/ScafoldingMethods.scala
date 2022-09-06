package chapter7

import cats.Monoid
import cats.syntax.monoid._
import cats.instances.list._

object ScafoldingMethods extends App {

  def map [A, B] (xs: List[A]) (f: A => B): List[B] =
    xs.foldRight (List.empty[B]) {
      (a, next) => f(a) :: next
    }

  def flatMap [A, B] (xs: List[A]) (f: A => List[B]): List[B] =
    xs.foldRight (List.empty[B]) {
      (a, next) => f(a) ::: next
    }

  def filter [A] (xs: List[A]) (p: A => Boolean): List[A] = 
    xs.foldRight (List.empty[A]) {
      (a, next) => if (p(a)) a :: next else next
    }

  def sum [A: Monoid] (xs: List[A]): A =
    xs.foldRight (Monoid[A].empty) { _ |+| _ }

  val xs = List(1, 2, 3, 4, 5, 6)

  println(
    map (xs) { _ * 2 }
  )

  println(
    flatMap (xs) { x => List(x, x + 1) }
  )

  println(
    filter (xs) { _ % 2 != 0 }
  )

  println(
    sum (map (xs) { x => List(x, x + 1) })
  )
}