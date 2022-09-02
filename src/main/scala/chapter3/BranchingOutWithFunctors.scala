package chapter3

import cats.Functor
import cats.syntax.functor._

object BranchingOutWithFunctors extends App {

  sealed trait Tree [+A]

  final case class Branch [A] (left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf [A] (value: A) extends Tree[A]

  object Tree {

    def branch [A] (left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf [A] (value: A): Tree[A] = Leaf(value)

    implicit def treeFunctor = new Functor[Tree] {
  
      def map [A, B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
        case Leaf(value)         => Leaf(f(value))
        case Branch(left, right) => Branch(
          map (left) (f),
          map (right) (f)
        )   
      }
    }
  }

  val t = Tree.branch(
    Tree.leaf(10),
    Tree.branch(
      Tree.leaf(1), Tree.leaf(20)
    )
  )

  println(t.map(_ * 2))
}