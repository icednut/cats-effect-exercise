package io.icednut.cats.effect.exercise.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, IO, IOApp}

import scala.concurrent.duration.FiniteDuration
import io.icednut.cats.effect.exercise.utils.*

import scala.concurrent.duration.*
import scala.util.Either

object RacingIOs extends IOApp.Simple:

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
        IO.sleep(duration) >>
        IO(s"computation for $value: done") >>
        IO(value)
    ).onCancel(IO(s"computation CANCELED for $value").debug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)

    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol) => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String]) // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */
  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = ???

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = ???

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = ???

  override def run: IO[Unit] = testRacePair().void
