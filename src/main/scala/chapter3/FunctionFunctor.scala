package chapter3

import cats.Functor

object FunctionFunctor {

  type Func [A, B] = A => B

  // Functor for Func[A, *]
  def funcFunctor [T] = new Functor[T => *] {

    def map [A, B] (ft: T => A) (f: A => B): T => B = 
      ft andThen f
  }
}