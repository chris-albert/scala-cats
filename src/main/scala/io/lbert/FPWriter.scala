package io.lbert

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.data.Writer
import cats.syntax.applicative._
import cats.instances.vector._
import cats.syntax.writer._
import cats.instances.vector._

object FPWriter extends App {

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 5.seconds)

  type Logged[A] = Writer[Vector[String], A]

  42.pure[Logged]

  Vector("Message").tell

  41.pure[Logged].map(_ + 1)

  def factorialWriter(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorialWriter(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  println(factorialWriter(5).run)
}
