package chapter9

import cats.Monoid
import cats.syntax.semigroup._
import cats.syntax.traverse._
import cats.instances.future

object FoldMapSingle {

  // Single-threaded map-reduce function
  def foldMap [A, B : Monoid] (v: Vector[A]) (f: A => B): B =
    v.foldLeft (Monoid.empty[B]) (_ |+| f(_))
    // v.map(f).fold (Monoid.empty[B]) (Monoid.combine[B])
}

object FoldMapPar extends App {

  import scala.concurrent.Future
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println(Runtime.getRuntime().availableProcessors())

  def parallelFoldMap [A, B : Monoid] (values: Vector[A]) (f: A => B): Future[B] = {

    val cores = Runtime.getRuntime().availableProcessors()
    val n = (values.size.toDouble / cores).ceil.toInt
    val batches = values.grouped(n).toVector
    def processBatch (batch: Vector[A]): Future[B] = Future {
      FoldMapSingle.foldMap (batch) (f)
    }

    batches.traverse(processBatch).map(
      FoldMapSingle.foldMap (_) (identity)
    )
  }

  val result: Future[Int] =
  parallelFoldMap((1 to 1000000).toVector)(identity)

  println(
    Await.result(result, 1.second)
  )
}

object FoldMapCats extends App {

  import scala.concurrent.Future
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import cats.Foldable
  import cats.Traverse
  import cats.Applicative
  import cats.syntax.functor._
  import cats.syntax.foldable._
  import cats.syntax.traverse._
  import cats.syntax.applicative._

  def foldMap [F[_]: Foldable, A, B : Monoid] (fa: F[A]) (f: A => B): B =
    fa.foldLeft (Monoid.empty[B]) (_ |+| f(_))

  def parFoldMap 
    [ F[_] : Foldable
    , L[_] : Traverse
    , G[_] : Applicative // Should be replaced with a type class that handles parallel computations 
    , A
    , B : Monoid] (batches: L[F[A]]) (f: A => B) (delay: B => G[B]): G[B] = {

      def processBatch (batch: F[A]): G[B] = delay {
        foldMap (batch) (f)
      } 

      batches.traverse(processBatch).map(
        foldMap (_) (identity)
      )
    }

  def vecFutureFoldMap [A, B : Monoid] (values: Vector[A]) (f: A => B): Future[B] = {

    val cores = Runtime.getRuntime().availableProcessors()
    val n = (values.size.toDouble / cores).ceil.toInt
    val batches = values.grouped(n).toVector
    parFoldMap(batches)(f)(Future(_))
  }

  val result: Future[Int] =
    vecFutureFoldMap((1 to 1000000).toVector)(identity)

  println(
    Await.result(result, 1.second)
  )
}