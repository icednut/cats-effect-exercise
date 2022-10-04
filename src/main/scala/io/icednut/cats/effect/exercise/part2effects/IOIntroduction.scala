package io.icednut.cats.effect.exercise.part2effects

import cats.effect.{ IO, Sync }

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    42
  }

  val aDelayedIO_v2: IO[Int] = IO {
    println("I'm producing an integer")
    42
  }

  val aDelayedIO_v3: IO[Int] = Sync[IO].delay {
    println("I'm producing an integer")
    42
  }

  // map, flatMap
  val improveMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram: IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply.*
  val combineMeaningOfLife = (ourFirstIO, improveMeaningOfLife).mapN {
    _ + _
  }
  def smallProgram_v2: IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /** Exercises
    */

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)
  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob
  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    iob.flatMap(_ => ioa)
  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // 3 - repeat an IO effects forever
  // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))
  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io)
  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v3(io)
  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion

    // 4 - convert an IO to a different type
    // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)
  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
//    ioa.map(_ => ())
    ioa.void

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    IO(n).flatMap { nn =>
      if (nn <= 0) IO.pure(0)
      else sumIO(nn - 1).map(_ + nn)
    }
  def sumIO_v2(n: Int): IO[Int] =
      if (n <= 0) IO.pure(0)
      else for {
        prevValue <- IO(n)
        nextValue <- sumIO(n - 1)
      } yield {
        prevValue + nextValue
      }

  // 7 - write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily
  // 1 1 2 3 5 8 13
  def fibonacci(n: Int): IO[BigInt] = {
    def run(pastValue: BigInt, nextValue: BigInt, index: Int): IO[BigInt] =
      if (index <= 0) IO.pure(pastValue)
      else run(nextValue, pastValue + nextValue, index - 1)

    run(0, 1, n)
  }
  def fibonacci_v2(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO(fibonacci(n - 1)).flatMap(x => x)
      prev <- IO(fibonacci(n - 2)).flatten
    } yield last + prev
  def fibonacci_v3(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1)) // same as .delay(...).flatten
      prev <- IO.defer(fibonacci(n - 2))
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
//    println(aDelayedIO.unsafeRunSync())
//    println(smallProgram.unsafeRunSync())
//    println(smallProgram_v2.unsafeRunSync())

//    println(sequenceTakeLast(aDelayedIO, improveMeaningOfLife).unsafeRunSync())
//    println(sequenceTakeFirst(aDelayedIO, improveMeaningOfLife).unsafeRunSync())
//    println(forever_v2(aDelayedIO).unsafeRunSync())
//    println(convert(aDelayedIO, 0).unsafeRunSync())
//    println(sumIO(10).unsafeRunSync())
    println(sumIO_v2(20000).unsafeRunSync())
//    println(sum(20000))
    println(fibonacci(11).unsafeRunSync())
    println(fibonacci_v2(11).unsafeRunSync())
  }

}
