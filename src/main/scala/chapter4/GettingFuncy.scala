package chapter4

/*
  In cats, Monad extends FlatMap which provides the
  `flatMap` function, and Applicative which provides the
  `pure` function. Applicative extends Functor

  - cats.syntax.flatMap provides syntax for flatMap
  - cats.syntax.functor provides syntax for map
  - cats.syntax.applicative provides syntax for pure
*/
object GettingFuncy {

  // Every monad is also a functor
  trait Monad [F[_]] {

    def pure [A] (a: A): F[A]

    def flatMap [A, B] (fa: F[A]) (f: A => F[B]): F[B]

    def map [A, B] (fa: F[A]) (f: A => B): F[B] =
      flatMap (fa) (f andThen pure)
  }
}