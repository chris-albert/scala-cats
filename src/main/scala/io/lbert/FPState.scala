package io.lbert

import cats.data.State
import State._
import cats.syntax.applicative._

object FPState extends App {


  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  println(a.run(10).value)
  println(a.runS(11).value)
  println(a.runA(12).value)

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  println(both.run(20).value)

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  println(program.run(1).value)

  type CalcState[A] = State[List[Int], A]

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case a :: b :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  val calcProgram = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  println(calcProgram.runA(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] = {

    input.foldLeft(0.pure[CalcState]) {
      (state, in) =>
        state.flatMap(_ => evalOne(in))
    }
  }

  val programAll = evalAll(List("1", "2", "+", "3", "*"))
  println(programAll.runA(Nil).value)

  val programMore = for {
    _   <- evalAll(List("1", "2", "+"))
    _   <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(programMore.runA(Nil).value)

}
