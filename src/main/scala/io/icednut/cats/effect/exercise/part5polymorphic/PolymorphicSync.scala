package io.icednut.cats.effect.exercise.part5polymorphic

import cats.Defer
import cats.effect.{IO, IOApp, MonadCancel, Sync}
import io.icednut.cats.effect.exercise.utils.general.*

import java.io.{BufferedReader, InputStreamReader}

object PolymorphicSync extends IOApp.Simple:

  val aDelayedIO = IO.delay { // "suspend computations in IO
    println("I'm an effect!")
    42
  }

  val aBlockingIO = IO.blocking { // on some specific thread pool for blocking computations
    println("loading...")
    Thread.sleep(1000)
    42
  }

  // synchronous computation
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](computation: => A): F[A] // "suspension" of a computation - will run on the Cats Effect thread pool
    def blocking[A](computation: => A): F[A] // runs on the blocking thread pool

    // defer comes for free
    def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)
  }

  val syncIO = Sync[IO] // given Sync[IO] in scope

  // abilities: pure, map/flatMap [여기까진 Monad], raiseError, uncancelable, + delay/block
  val aDelayedIO_v2 = syncIO.delay {
    println("I'm an effect!")
    42
  } // same as IO.delay

  val aBlockingIO_v2 = syncIO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  }

  val aDeferredIO = IO.defer(aDelayedIO)

  /**
   * Exercise: write a polymorphic console
   */
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine(): F[String]
  }

  object Console {

    // my answer
    def apply[F[_]](using sync: Sync[F]): Console[F] =
      new Console[F]() {
        override def println[A](a: A): F[Unit] = sync.delay {
          scala.Predef.println(a)
        }

        override def readLine(): F[String] = sync.blocking {
          scala.Console.in.readLine()
        }
      }

    import cats.syntax.functor.*
    def make[F[_]](using sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {
        def println[A](a: A): F[Unit] =
          sync.blocking(out.println(a))

        def readLine(): F[String] = {
          val bufferedReader = new BufferedReader(new InputStreamReader(in))

          sync.interruptible(bufferedReader.readLine())

          /*
            There's a potential problem hanging one of the threads from the blocking thread pool
              (or - oh my! - one of the CE threads).

            There's also sync.interruptible(true/false) which attempts to block the thread via thread interrupts in case of cancel
            The flag tells whether you want the thread interrupt signals to be sent repeatedly (true) or not (false).
           */
        }
      }
    }
  }

  def consoleReader() = for {
    console <- Console.make[IO]
    _ <- console.println("Hi, what's your name?")
    name <- console.readLine()
    _ <- console.println(s"Hi $name, nice to meet you")
  } yield ()

//  override def run: IO[Unit] = Console[IO].println("Hello Cats Effect, Polymorphic") >> Console[IO].readLine().debug.void
  override def run: IO[Unit] = consoleReader()