package io.icednut.cats.effect.exercise.part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.Poll
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import io.icednut.cats.effect.exercise.utils.general.*
import scala.concurrent.duration.*

object PolymorphicCancellation extends IOApp.Simple:

  trait MyApplicative[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicative[F, E] with Monad[F]

  // MonadCancel
  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  // monadCancel for IO
  given monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values
  val molIO: IO[Int]          = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("once started, I can't go back...")
      res <- monadCancelIO.pure(56)
    } yield res
  }

  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    for {
      _ <- mc.pure("once started, I can't go back...")
      res <- mc.pure(56)
    } yield res
  }

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener    = mustCompute.onCancel(IO("I'm being cancelled!").void)
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled!").void)
  // .onCancel as extension method
  import cats.effect.syntax.monadCancel.* // .onCancel

  // allow finalizers
  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"successful: ${a}").void)
    case Errored(ex)   => IO(s"failed: ${ex}").void
    case Canceled()    => IO("canceled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsage =
    monadCancelIO.bracket(IO(42))(value => IO(s"Using the meaning of life: $value"))(value =>
      IO("releasing the meaning of life...").void
    )

  /**
   * Exercise - generalize a piece of code
   */
  // hint: use this instead of IO.sleep
  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // not semantic blocking

  // my answer
  def authFlowGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Unit] = {
    val inputPassword: F[String] = mc.pure("Input password: ").debug >> mc.pure("(typing password)").debug.unsafeSleep(1.seconds) >> mc.pure("RockTheJVM1!")
    val verifyPassword: String => F[Boolean] = (pw: String) => mc.pure("verifying...").debug.unsafeSleep(1.second) >> mc.pure(pw == "RockTheJVM1!")

    val authFlow: F[Unit] = mc.uncancelable { (poll: Poll[F]) =>
      for {
        pw <- poll(inputPassword).onCancel(mc.pure("Authentication timed out. Try again later.").debug.void) // this is cancelable
        verified <- verifyPassword(pw) // this is NOT cancelable
        _ <- if (verified) mc.pure("Authentication successful.").debug else mc.pure("Authentication failed.").debug // this is NOT cancelable
      } yield ()
    }

    authFlow
  }

  // rockthejvm answer
  def inputPassword[F[_], E](using mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("Input password: ").debug
    _ <- mc.pure("(typing password)").debug
    _ <- unsafeSleep(1.seconds)
    pwd <- mc.pure("RockTheJVM1!")
  } yield pwd

  def verifyPassword[F[_], E](pw: String)(using mc: MonadCancel[F, E]): F[Boolean] =
    for {
      _ <- mc.pure("verifying...").debug
      _ <- unsafeSleep(1.second)
      result <- mc.pure(pw == "RockTheJVM1!")
    } yield result

  def authFlow[F[_], E](using mc: MonadCancel[F, E]): F[Unit] = mc.uncancelable { (poll: Poll[F]) =>
    for {
      pw <- poll(inputPassword).onCancel(mc.pure("Authentication timed out. Try again later.").debug.void) // this is cancelable
      verified <- verifyPassword(pw) // this is NOT cancelable
      _ <- if (verified) mc.pure("Authentication successful.").debug else mc.pure("Authentication failed.").debug // this is NOT cancelable
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow[IO, Throwable].start
    _ <- IO.sleep(3.second) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  //override def run = authFlowGeneral[IO, Throwable]
  override def run = authProgram
