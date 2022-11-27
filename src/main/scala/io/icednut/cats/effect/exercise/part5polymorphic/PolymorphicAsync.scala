package io.icednut.cats.effect.exercise.part5polymorphic

import cats.effect.kernel.Concurrent
import cats.effect.{Async, IO, IOApp, Sync, Temporal}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import io.icednut.cats.effect.exercise.utils.general.*

object PolymorphicAsync extends IOApp.Simple:

  // Async - asynchronous computation, "suspended" in F
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // fundamental description of async computation
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => IO[Option[IO[Unit]]]): F[A]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

    // never-ending effect
//    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
//      async(kb => map(pure(cb(kb)))(_ => None))
//    def never[A]: F[A] = {
//      async_ { cb =>
//        ()
//      }
//    }
  }

  val asyncIO = Async[IO]

  // pure, map/flatMap, raiseError, uncancelable, start, ref/deferred, sleep, delay/defer/blocking, *
  val ec = asyncIO.executionContext

  // power: async_ + async: Foreign Function Interface
  val threadPool = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit
  val asyncMeaningOfLife: IO[Int] = IO.async_ { (cb: Callback[Int]) =>
    // start computation an some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  }
  val asyncMeaningOfLife_v2: IO[Int] = asyncIO.async_ { (cb: Callback[Int]) =>
    // start computation an some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  }

  val asyncMeaningOfLifeComplex: IO[Int] = IO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Canceled...").debug.void)) // finalizer in case the computation gets cancelled
  }

  val asyncMeaningOfLifeComplex_v2: IO[Int] = asyncIO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Canceled...").debug.void)) // finalizer in case the computation gets cancelled
  }

  val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  val asyncMeaningOfLife_v3 = asyncIO.evalOn(IO(42).debug, myExecutionContext).guarantee(IO(threadPool.shutdown()))

  /**
   * Exercises
   * 1 - implement never and async_ in terms of the big async.
   * 2 - tuple two effects with different requirements.
   */
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  def firstEffect[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def secondEffect[F[_]: Sync, A](a: A): F[A] = Sync[F].pure(a)

  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] =
    for {
      fe <- firstEffect(a)
      se <- secondEffect(a)
    } yield (fe, se)

  override def run: IO[Unit] = ???
