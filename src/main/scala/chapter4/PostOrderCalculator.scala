package chapter4

import cats.data.State
import cats.syntax.flatMap._

object PostOrderCalculator extends App {

  type CalcState [A] = State[List[Int], A]

  def evalOne (symbol: String): CalcState[Int] = State {
    stack => (stack, symbol) match {
      case (x :: y :: stack, "+") => (x + y :: stack, x + y) 
      case (x :: y :: stack, "*") => (x * y :: stack, x * y) 
      case (x :: y :: stack, "-") => (x - y :: stack, x - y) 
      case (x :: y :: stack, "/") => (x / y :: stack, x / y) 
      case (stack, symbol)        => symbol.toIntOption match {
        case None    => throw new IllegalArgumentException("Symbols should only be integers or the following operations: +, *, -, /")
        case Some(n) => (n :: stack, n)
      }
      case _                      => throw new Exception(
        "The stack is in the wrong configuration"
      )
    }
  }

  println(evalOne("42").runA(Nil).value)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    r <- evalOne("+")
  } yield r

  println(program.runA(Nil).value)

  def evalAll (input: List[String]): CalcState[Int] = input match {
    case Nil          => throw new Exception("Symbol list is empty")
    case s :: symbols => symbols.foldLeft (evalOne(s)) {
      _ >> evalOne(_)
    }
  }

  val multistageProgram = evalAll(
    List("1", "2", "+", "3", "*")
  )

  println(
    multistageProgram.runA(Nil).value
  )

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    r <- evalOne("*")
  } yield r

  println(
    biggerProgram.runA(Nil).value
  )

  def evalInput (input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(
    evalInput("1 2 + 3 4 + *")
  )
}