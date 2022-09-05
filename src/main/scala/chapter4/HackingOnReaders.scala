package chapter4

import cats.data.Reader
import cats.syntax.applicative._

object HackingOnReaders extends App {

  final case class Db (
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader [A] = Reader[Db, A]

  def findUsername (userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword (username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin (userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap { maybeUsername =>
      
      val empty = false.pure[DbReader]
      maybeUsername.fold (empty) {
        checkPassword(_, password)  
      }  
    }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(
    checkLogin(1, "zerocool").run(db)
  )

  println(
    checkLogin(4, "davinci").run(db)
  )
}