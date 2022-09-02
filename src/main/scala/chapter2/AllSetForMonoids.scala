package chapter2

object AllSetForMonoids {

  import cats.Monoid
  import cats.Semigroup

  implicit def setIntersectionSemigroup [A] = new Semigroup[Set[A]] {

    def combine (x: Set[A], y: Set[A]): Set[A] = x intersect y
  }

  implicit def setUnionMonoid [A] = new Monoid[Set[A]] {

    def combine (x: Set[A], y: Set[A]): Set[A] = x union y

    def empty: Set[A] = Set.empty
  }

  implicit def setSymDiffMonoid [A] = new Monoid[Set[A]] {

    def combine (x: Set[A], y: Set[A]): Set[A] = 
      (x union y) diff (x intersect y)

    def empty: Set[A] = Set.empty
  }
}