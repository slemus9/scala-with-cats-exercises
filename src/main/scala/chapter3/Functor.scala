package chapter3

/*
  Laws:
  
  identity: 
    map (fa) (x => x) = fa
  composition: 
    map (fa) (g compose f) = (map _ g) compose (map _ f)
*/
trait Functor [F[_]] {

  def map [A, B] (fa: F[A]) (f: A => B): F[B]
}