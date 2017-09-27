package io.lbert

import cats.Cartesian
import cats.instances.option._
import cats.syntax.cartesian._

object FPCartesian extends App {

  trait MyCartesian[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  println(Cartesian[Option].product(Some(123), Some("123")))
  println(Cartesian[Option].product(None, Some("abc")))
  println(Cartesian[Option].product(Some(123), None))

  println(Cartesian.tuple3(Option(1), Option(2), Option(3)))
  println(Cartesian.tuple3(Option(1), Option(2), Option.empty[Int]))

  println(Cartesian.map3(
    Option(1),
    Option(2),
    Option(3)
  )(_ + _ + _))

  println(Cartesian.map3(
    Option(1),
    Option(2),
    Option.empty[Int]
  )(_ + _ + _))

  println((Option(123) |@| Option("abc")).tupled)

  case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield") |@|
    Option(1978)       |@|
    Option("Orange and black")
  ).map(Cat.apply)
}
