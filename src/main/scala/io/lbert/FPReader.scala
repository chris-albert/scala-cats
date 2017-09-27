package io.lbert

import cats.data.Reader
import cats.syntax.option._
import cats.syntax.applicative._

object FPReader extends App {

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat,String] =
    Reader(cat => cat.name)

  private val garfield = Cat("Garfield", "lasagne")
  println(catName.run(garfield))

  val greetKitty: Reader[Cat,String] =
    catName.map(name => s"Hello $name")

  private val healthcliff = Cat("Healthcliff", "junk food")
  println(greetKitty.run(healthcliff))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      msg1 <- greetKitty
      msg2 <- feedKitty
    } yield s"$msg1 $msg2"

  println(greetAndFeed(garfield))
  println(greetAndFeed(healthcliff))

  case class Db(usernames: Map[Int, String],
                passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String,
                    password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int,
                 password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      out      <- username.map { username =>
                    checkPassword(username, password)
                  }.getOrElse(false.pure[DbReader])
    } yield out


  val db = Db(
    Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    ), Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
  )

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))

}
