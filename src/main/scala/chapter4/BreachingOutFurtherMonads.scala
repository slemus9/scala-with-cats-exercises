package chapter4

import cats.Monad
import cats.syntax.flatMap._

object BreachingOutFurtherMonads extends App {

  sealed trait Tree [+A]

  final case class Branch [A] (
    left: Tree[A], right: Tree[A]
  ) extends Tree[A]

  final case class Leaf [A] (value: A) extends Tree[A]

  def branch [A] (left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf [A] (value: A): Tree[A] = Leaf(value)

  object Tree {

    def tailRecMNonTail [A, B] (a: A) (f: A => Tree[Either[A, B]]): Tree[B] = 
      f(a) match {
        case Leaf(Left(a))  => tailRecMNonTail (a) (f)
        case Leaf(Right(b)) => Leaf(b) 
        case Branch(l, r)   => 
          def next (t: Tree[Either[A, B]]) = treeMonad.flatMap (t) { 
            case Left(a)  => tailRecMNonTail (a) (f)
            case Right(b) => Leaf(b)
          }

          Branch(next(l), next(r))
      }


    implicit def treeMonad = new Monad[Tree] {

      def flatMap [A, B] (t: Tree[A]) (f: A => Tree[B]): Tree[B] = t match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => Branch(
          flatMap (l) (f),
          flatMap (r) (f)
        )
      } 

      // TODO: Try to do it tail recursive
      def tailRecM [A, B] (a: A) (f: A => Tree[Either[A, B]]): Tree[B] = 
        tailRecMNonTail (a) (f)

      def pure [A] (x: A): Tree[A] = leaf(x)
    }
  }

  val t = branch(
    leaf(1),
    branch(leaf(2), leaf(3))
  )

  println(
    t.flatMap(x => branch(
      leaf(x), leaf(x + 1)
    ))
  )
}