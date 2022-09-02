import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._

object AddingAllTheThings extends App {

  def addIntItems (items: List[Int]): Int = 
    items.foldLeft (Monoid[Int].empty) (_ |+| _)

  def addOptionalIntItems (items: List[Option[Int]]): Option[Int] = 
    items.foldLeft (Monoid[Option[Int]].empty) (_ |+| _)

  def addItems [A: Monoid] (items: List[A]): A =
    items.foldLeft (Monoid.empty[A]) (_ |+| _)

  final case class Order(totalCost: Double, quantity: Double)

  object Order {

    implicit val orderMonoid = new Monoid[Order] {

      def combine (o1: Order, o2: Order): Order = Order(
        o1.totalCost |+| o2.totalCost,
        o1.quantity  |+| o2.quantity
      )

      def empty: Order = Order(0, 0)
    }
  }

  println(
    addItems(List(1.some, 10.some, none, 11.some))
  )

  println(addItems(
    List(Order(10.5, 2), Order(14.2, 1), Order(11.0, 5))
  ))
}
