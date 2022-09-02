package chapter2

object TruthAboutMonads {

  // associativity: (x combine y) combine z = x combine (y combine z)
  trait Semigroup [A] {

    def combine (x: A, y: A): A
  }

  /*
    Is a Monoid and has the following properties
    left identity of combine:  
      empty combine x = x
    right identity of combine: 
      x combine empty = x
  */
  trait Monoid [A] extends Semigroup[A] {

    def empty: A
  }

  object Monoid {

    def apply [A] (implicit monoid: Monoid[A]) =
      monoid
  }

  object MonoidInstances {

    /*
      false || b = b
      true || b  = true
    */
    implicit val boolOrMonoid = new Monoid[Boolean] {

      def combine (x: Boolean, y: Boolean): Boolean = x || y

      def empty: Boolean = true
    }

    /*
      true && b  = b
      false && b = false
    */
    implicit val boolAndMonoid = new Monoid[Boolean] {

      def combine (x: Boolean, y: Boolean): Boolean = x && y

      def empty: Boolean = false
    }
  
    /*
      b ^ false = b
      false ^ b = b 
      _ ^ _     = false
    */
    implicit val boolXorMonoid = new Monoid[Boolean] {

      def combine (x: Boolean, y: Boolean): Boolean = x ^ y

      def empty: Boolean = false
    }

    /*
      true iff true   = true
      false iff false = true
      _ iff _         = false

      true iff b = b
      b iff true = b
    */
    implicit val boolIffMonoid = new Monoid[Boolean] {

      def combine(x: Boolean, y: Boolean): Boolean = ! (x ^ y)

      def empty: Boolean = true
    }
  }
}