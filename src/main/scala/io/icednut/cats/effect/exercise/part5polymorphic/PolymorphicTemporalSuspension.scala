package io.icednut.cats.effect.exercise.part5polymorphic

import cats.effect.kernel.Concurrent
import cats.effect.{IO, IOApp, Temporal}

import scala.concurrent.duration.FiniteDuration
import io.icednut.cats.effect.exercise.utils.general.*

import scala.concurrent.TimeoutException
import scala.concurrent.duration.*

object PolymorphicTemporalSuspension extends IOApp.Simple:

  // Temporal - time-blocking effects
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks this fiber for a specified time
  }

  // abilities: map/flatMap, raiseError, uncancelable, start, ref/deferred, +sleep
  val temporalIO = Temporal[IO] // given Temporal[IO] in scope
  val chainOfEffects = IO("Loading...").debug *> IO.sleep(1.seconds) *> IO("Game Ready!").debug
  val chainOfEffects_v2 = temporalIO.pure("Loading...").debug *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready!").debug

  /**
   * Exercise: generalize the following piece
   */
  //  import cats.syntax.flatMap.*
  //  def timeout[F[_], A](io: F[A], duration: FiniteDuration)(using temporal: Temporal[F]) : F[A] = {
  //    temporal.race(
  //      temporal.sleep(duration) >> temporal.raiseError[Throwable](new TimeoutException()),
  //      io
  //    ).flatMap {
  //      case Left(ex) => temporal.raiseError[Throwable](ex)
  //      case Right(result) => temporal.pure(result)
  //    }
  //  }


  //  override def run: IO[Unit] = timeout[IO, Unit](  IO.sleep(3.second) >> IO("Hello cats effect!").void, 1.second)
  override def run: IO[Unit] = ???