package chapter7

object ReflectingOnFolds extends App {

  val xs = List(1, 2, 3, 4, 5, 6)
  val nil = List.empty[Int]

  // Reconstructs the list in the same order
  println(
    xs.foldRight (nil) { _ :: _ }
  )

  // Reverses the list
  println(
    xs.foldLeft (nil) { (acc, x) => x :: acc }
  )
}