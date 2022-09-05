package chapter4

import cats.data.State

object StateMonad extends App {

  /*
    State [S, A] represent functions of type S => (S, A),
    where S is the type of the state and A is the type of
    the result.

    An instance of State is a function that transforms an
    input state to an output state, and computes a result
  */
  val a = State[Int, String] {
    state => (state, s"The state is $state")
  }

  // To access the state and result, State uses Eval
  val (state, result) = a.run(10).value

  val justTheState = a.runS(10).value

  val justTheResult = a.runA(10).value

  // Composing and transforming state
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

  println(
    both.run(20).value
  )

  // Extracts the state as the result
  val getDemo = State.get[Int]

  println(
    getDemo.run(10).value
  )

  // Updates the state and returns unit as the result
  val setDemo = State.set[Int](30)

  println(
    setDemo.run(10).value
  )

  // Does nothing with the state and returns the supplied result
  val pureDemo = State.pure[Int, String]("Result")

  println(
    pureDemo.run(10).value
  )

  // Transforms the result using a function that takes the state as parameter
  val inspectDemo = State.inspect[Int, String] {
    x => s"$x!"
  }

  println(
    inspectDemo.run(10).value
  )

  // Updates the state using the supplied function
  val modifyDemo = State.modify[Int](_ + 1)

  println(
    modifyDemo.run(10).value
  )

  import State._

  val program = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  println(program.run(1).value)
}