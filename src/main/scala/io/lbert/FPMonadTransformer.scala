package io.lbert

import cats.data.{OptionT, Writer}
import cats.Monad
import cats.instances.list._
import cats.syntax.applicative._
import cats.data.EitherT
import scala.concurrent.Future
import cats.instances.future._
import cats.syntax.flatMap._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._


object FPMonadTransformer extends App {

  type ListOption[A] = OptionT[List, A]

  val result = 42.pure[ListOption]
  println(result)

  val a = 10.pure[ListOption]
  val b = 32.pure[ListOption]

  val c = a flatMap { (x: Int) =>
    b map { (y: Int) =>
      x+y }
  }

  println(c)

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

  def addNumbers(
                  a: String,
                  b: String,
                  c: String
                ): Logged[Option[Int]] = {
    import cats.data.OptionT
    // Transform the incoming stacks to work on them:
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    // Return the untransformed monad stack:
    result.value
  }
  // This approach doesn't force OptionT on other users' code:
  println(addNumbers("1", "2", "3"))
  println(addNumbers("1", "a", "3"))

  type Response[A] = EitherT[Future,String,A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(ally: String): Response[Int] = {
    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None      => EitherT.left(Future(s"$ally unreachable"))
    }
  }

  def canSpecialMove(ally1: String,
                     ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2) > 15

  def tacticalReport(ally1: String,
                     ally2: String): String =
    Await.result(
      canSpecialMove(ally1,ally2).value,
      1.second
    ) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
