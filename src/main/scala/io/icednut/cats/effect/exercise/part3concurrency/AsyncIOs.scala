package io.icednut.cats.effect.exercise.part3concurrency

import cats.effect.{ IO, IOApp }

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }
import io.icednut.cats.effect.exercise.utils.*

import scala.concurrent.duration.*

object AsyncIOs extends IOApp.Simple:

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool           = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread...")
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLifeEither())
    // Big Problem

  // lift computation to an IO
  // async is a Foreign Function Interface
  val asyncMolIO: IO[Int] = IO.async_ {
    (cb: Callback[Int]) => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
      threadPool.execute { () => // computation not managed by CE
        val result: Either[Throwable, Int] = computeMeaningOfLifeEither()
        cb(result) // CE thread is notified with the result
      }
  }

  /** Exercises: lift an async computation on ec to an IO.
    */
  // my answer
//  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO
//    .async_ { (cb: Callback[A]) =>
//      val result: Either[Throwable, A] = Try(computation()).toEither
//
//      cb(result)
//    }
//    .evalOn(ec)
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO.async_[A] { (cb: Callback[A]) =>
    ec.execute { () =>
      val result = Try(computation()).toEither
      cb(result)
    }
  }

  val asyncMolIO_v2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  /** Exercise: lift an async computation as a Future, to an IO.
    */
  // my answer
  def intFutureToIO(future: Future[Int])(ec: ExecutionContext): IO[Int] = IO.async_[Int] { (cb: Callback[Int]) =>
    future.onComplete { (result: Try[Int]) =>
      cb(result.toEither)
    }
  }
  // rockthejvm answer
  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { (cb: Callback[A]) =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }
    }

  lazy val molFuture: Future[Int] = Future(computeMeaningOfLife())
  val asyncMolIO_v3: IO[Int] = convertFutureToIO(molFuture)
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  /**
   * Exercise: a never-ending IO?
   */
  // my answer
  val neverEndingIO: IO[Int] = IO.async_[Int] { (cb: Callback[Int]) =>
    ()
  }
  // rockthejvm answer: same my answer
  // no callback, no finish
  val neverEndingIO_v2: IO[Int] = IO.never

  /*
    FULL ASYNC CALL
   */
  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      /*
        finalizer in case computation gets cancelled.
        finalizers are of type IO[Unit]
        not specifying finalizer => Option[IO[Unit]]
        creating option is an effect => IO[Option[IO[Unit]]]
       */
      // return IO[Option[IO[Unit]]]
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      } // IO[Unit]
        .as(Some(IO("Cancelled!").debug.void))

    }

//    asyncMeaningOfLifeIO_v2.void
    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

//  override def run = asyncMolIO_v2.debug >>
//    IO(threadPool.shutdown())

  override def run = demoAsyncCancellation().debug >>
      IO(threadPool.shutdown())
