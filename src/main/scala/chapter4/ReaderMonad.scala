package chapter4

import cats.data.Reader

object ReaderMonad extends App {

  final case class Cat (name: String, favoriteFood: String)

  //Instances of reader wrap up functions of one argument
  val catName: Reader[Cat, String] = Reader(_.name) // ReaderT[Id, A, B]

  // Retreive the function again
  println(
    catName.run(Cat("Garfield", "lasagne")) // Id[String]
  )

  val greetKitty: Reader[Cat, String] =
    catName.map { name => s"Hello $name" }

  println(
    greetKitty.run(
      Cat("Heathcliff", "junk food")
    )
  )

  val feedKitty: Reader[Cat, String] = Reader {
    cat => s"Have a nice bowl of ${cat.favoriteFood}"
  }

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."

  println(
    greetAndFeed.run(
      Cat("Garfield", "lasagne")
    )
  )
}