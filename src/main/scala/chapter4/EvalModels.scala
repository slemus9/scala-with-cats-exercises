package chapter4
  
import cats.Eval
import scala.math

/*
  Eval provides eager and lazy evaluation, together 
  with stack safety

  Subtypes:
    Now - call by value
    Always - call by name
    Later - call by need
*/
object EvalModels extends App {

  // eager
  val now = Eval.now(math.random + 1000)

  // lazy but not memoized
  val always = Eval.always(math.random + 3000)

  // lazy and memoized
  val later = Eval.later(math.random + 2000)

  // Mapping functions are always called lazily
  // regardless of the initial Eval
  val greeting = Eval
    .always { println("Step1"); "Hello" }
    .map { s => println("Step2"); s"$s world" }

  println(greeting.value)
  println("------------------")

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }

  println(ans.value)
  println("------------------")


  // `memoize` allows us to memoize a chain of computations
  // calculations after the call retain their original semantics
  val saying = Eval
    .always { println("Step1"); "The cat" }
    .map { s => println("Step 2"); s"$s sat on" }
    .memoize
    .map { s => println("Step 3"); s"$s the mat" }

  println("First access:")
  println(saying.value)
  println("Second access:")
  println(saying.value)

  // `map` and `flatMap` are trampolined - stack safety
  // `defer`takes an Eval and defers its evaluation 

  def factorial (n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(n)
    else Eval.defer(
      factorial(n - 1).map(_ * n)
    )

  println(factorial(50000).value)
  println("------------------")
}