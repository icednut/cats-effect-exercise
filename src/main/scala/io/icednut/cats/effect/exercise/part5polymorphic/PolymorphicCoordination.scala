package io.icednut.cats.effect.exercise.part5polymorphic

import cats.effect.{MonadCancel, Concurrent, Deferred, IO, IOApp, Ref, Spawn}
import io.icednut.cats.effect.exercise.utils.general.*

import scala.concurrent.duration.*

object PolymorphicCoordination extends IOApp.Simple :

  // Concurrent = Ref + Deferred for ANY effect type
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO] // given instance of Concurrent[IO], type class instance
  val aDeferred = Deferred[IO, Int] // given/implicit Concurrent[IO] in scope
  val aDeferred_v2 = concurrentIO.deferred[Int]
  val aRef = concurrentIO.ref(42)

  // capabilities: pure, map/flatMap, raiseError,

  def eggBoiler(): IO[Unit] = {
    def eggReadyNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("Egg boiling on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- IO("EGG READY!!").debug
    } yield ()

    def tickingClock(ticks: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- IO(count).debug
      _ <- if (count >= 10) signal.complete(()) else tickingClock(ticks, signal)
    } yield ()

    for {
      signal <- Deferred[IO, Unit]
      tickRef <- Ref[IO].of[Int](0)
      fibEggReady <- eggReadyNotification(signal).start
      fibTick <- tickingClock(tickRef, signal).start
      _ <- fibEggReady.join
      _ <- fibTick.join
    } yield ()
  }

  import cats.syntax.flatMap.* // flatMap
  import cats.syntax.functor.* // map
  import cats.effect.syntax.spawn.*

  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // not semantic blocking

  def polymorphicEggBoiler[F[_]](using concurrent: Concurrent[F]): F[Unit] = {
    def eggReadyNotification(signal: Deferred[F, Unit]) = for {
      _ <- concurrent.pure("Egg boiling on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- concurrent.pure("EGG READY!!").debug
    } yield ()

    def tickingClock(counter: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep[F, Throwable](1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- concurrent.pure(count).debug
      _ <- if (count >= 10) signal.complete(()).void else tickingClock(counter, signal)
    } yield ()

    for {
      tickRef <- concurrent.ref(0)
      signal <- concurrent.deferred[Unit]
      fibEggReady <- eggReadyNotification(signal).start
      fibTick <- tickingClock(tickRef, signal).start
      _ <- fibEggReady.join
      _ <- fibTick.join
    } yield ()
  }

  /**
   * Exercise:
   * 1. generalize ourRacePair
   * 2. Generalize the Mutex concurrency primitive for any F
   */


  override def run: IO[Unit] = polymorphicEggBoiler[IO]