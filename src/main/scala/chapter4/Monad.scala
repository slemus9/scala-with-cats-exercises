package chapter4

/*
  Laws:
  
  left identity:
    (pure a) flatMap f = f(a)
  right identity:
    ma flatMap pure = ma
  associativity:
    (ma flatMap f) flatMap g = ma flatMap (a => f(a) flatMap g)
*/
trait Monad [F[_]] {

  def pure [A] (value: A): F[A]

  def flatMap [A, B] (value: F[A]) (f: A => F[B]): F[B]
}