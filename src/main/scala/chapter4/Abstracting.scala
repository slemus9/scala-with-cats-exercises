package chapter4

object Abstracting {

  import cats.MonadError

  def validateAdult [F[_]] (age: Int) (implicit F: MonadError[F, Throwable]): F[Int] = 
    if (age >= 18) F.pure(age)
    else F.raiseError(new IllegalArgumentException("Age must be greater or equal to 18"))
  
}