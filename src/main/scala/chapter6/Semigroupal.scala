package chapter6

/*
  Laws:
  associativity:
    (a product b) product c = a product (b product c)
*/
trait Semigroupal [F[_]] {

  // fa and fb are independent computations
  def product [A, B] (fa: F[A], fb: F[B]): F[(A, B)]
}