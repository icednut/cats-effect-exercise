package io.icednut.cats.effect.exercise.part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, GenSpawn, IO, IOApp, MonadCancel, Outcome, Spawn}
import cats.implicits.catsSyntaxApply

object PolymorphicFibers extends IOApp.Simple :

  // Spawn = create fibers for any effect
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // creates a fiber
    def never[A]: F[A] // a forever-suspending effect
    def cede: F[Unit] // a "yield" effect

    // TODO
  }

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  val mol = IO(42)
  val fiber: IO[Fiber[IO, Throwable, Int]] = mol.start

  // pure, map/flatMap, raiseError, uncancelable, start

  given spawnIO: Spawn[IO] = Spawn[IO] // fetch the given/implicit Spawn[IO]

  def ioOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- spawnIO.start(io) // io.start assumes the presence of a Spawn[IO]
    result <- fib.join
  } yield result

  import cats.syntax.functor.*
  import cats.syntax.flatMap.*

  // generalize
  import cats.effect.syntax.spawn.*
  def effectOnSomeThread[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
    fib <- spawn.start(fa) // io.start assumes the presence of a Spawn[IO]
    result <- fib.join
  } yield result

  val molOnFiber = ioOnSomeThread(mol)
  val molOnFiber_v2 = effectOnSomeThread(mol)

  /**
   * Exercise - generalize the following code
   */
  def ioRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(valueA) => fibB.cancel >> valueA.map(a => Left(a))
        case Errored(e) => fibB.cancel >> IO.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
        case Errored(e) => fibA.cancel >> IO.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both Computations canceled."))
        }
      }
    }
  }

  def generalRace[F[_], A, B](fa: F[A], fb: F[B])(using spawn: Spawn[F]): F[Either[A, B]] = {
    spawn.racePair(fa, fb).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(valueA) => fibB.cancel.flatMap { _ =>
          valueA.map(a => Left(a))
        }
        case Errored(e) => fibB.cancel.flatMap { _ => spawn.raiseError(e) }
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel.flatMap {_ => effectB.map(b => Right(b)) }
        case Errored(e) => fibA.cancel.flatMap {_ => spawn.raiseError(e)}
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both Computations canceled."))
        }
      }
    }
  }

  import scala.concurrent.duration.*
  import io.icednut.cats.effect.exercise.utils.*

  val fast = IO.sleep(1.second) >> IO(42).debug
  val slow = IO.sleep(2.second) >> IO("Scala").debug
  val race = ioRace(fast, slow)
  val race_v2 = generalRace(fast, slow)

//  override def run: IO[Unit] =
//    generalRace[IO, Int, Int](
//      fa = IO.sleep(2.seconds) >> IO(34),
//      fb = IO.sleep(1.second) >> IO(56)
//    ).void
  override def run: IO[Unit] = race.void

