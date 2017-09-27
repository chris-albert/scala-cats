package io.lbert

import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._
import cats.instances.string._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object PygmyHadoop extends App {

  def foldMap[A,B: Monoid](seq: Vector[A])(f: A => B):B =
    seq.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  println("Processors: " + Runtime.getRuntime.availableProcessors)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {

    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    val groups: Iterator[Vector[A]] = values.grouped(groupSize)

    val futures: Iterator[Future[B]] =
      groups.map{ group =>
        Future {
          group.foldLeft(Monoid[B].empty)(_ |+| func(_))
        }
      }

    Future.sequence(futures).map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  println(Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 1.second))
}
