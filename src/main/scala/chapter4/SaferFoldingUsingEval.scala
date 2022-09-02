package chapter4

import cats.Eval

object SaferFoldingUsingEval extends App {

  def foldRight [A, B] (xs: List[A], z: B) (f: (A, B) => B): B = {

    def go (xs: List[A]): Eval[B] = xs match {
      case Nil     => Eval.now(z)
      case x :: xs => Eval.defer(
        go(xs).map(f(x, _))
      )
    }

    go(xs).value
  }

  println(
    foldRight((1 to 100000).toList, 0L)(_ + _)
  )
}