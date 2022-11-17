package io.icednut.cats.effect.exercise.part3concurrency

import cats.effect.kernel.Outcome.*
import cats.effect.{ IO, * }

import scala.concurrent.duration.*

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO(42)
  val favlang       = IO.pure("Scala")

  import io.icednut.cats.effect.exercise.utils._
  def sameThreadIOs() = for {
    mol <- meaningOfLife.debug
    lang <- favlang.debug
  } yield ()

  // introduce the Fiber
  // almost impossible to create fibers manually
  def createFiber: Fiber[IO, Throwable, String] = ???

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favlang.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // join
  } yield result

  /*
    IO[ResultType of fib.join]
    fib.join =  Outcome[IO, Throwable, A]
   */

  /*
    possible outcomes:
    - success with an IO
    - failure with an exception
    - cancelled
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e)        => IO(0)
    case Canceled()        => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Unit] = {
    val task                                    = IO("starting").debug >> IO.sleep(1.seconds) >> IO("done").debug
    val taskWithCancellationHandler: IO[String] = task.onCancel(IO("I'm being cancelled!").debug.void)

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
//      _ <- IO.sleep(500.millis) >> IO("cancelling").debug
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /** Exercises:
    *   1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
    *      - return the result is an IO
    *      - if errored or cancelled, return a failed IO
    *
    * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing
    * both result
    *   - if both IOs complete successfully, tuple their results
    *   - if the first IO returns an error, raise the error (ignoring the second IO's result/error)
    *   - if the first IO doesn't error but second IO returns an error, raise that error
    *   - if one (or both) cancelled, raise a RuntimeException
    *
    * 3. Write a function that adds a timeout to an IO:
    *   - IO runs on a fiber
    *   - if the timeout duration passes, then the fiber is cancelled
    *   - the method returns an IO[A] which contains
    *     - the original value if the computation is successful before the timeout signal
    *     - the exception if the computation is failed before the timeout signal
    *     - a RuntimeException if it times out (i.e. cancelled by the timeout)
    */
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    // my answer
//    for {
//      ioFiber <- io.start
//      resultOutcome <- ioFiber.join
//      result <- resultOutcome match {
//                  case Canceled()      => IO.raiseError[A](new RuntimeException("Computation cancelled"))
//                  case Errored(ex)     => IO.raiseError[A](ex)
//                  case Succeeded(v: A) => IO.pure[A](v)
//                }
//    } yield result

    val ioResult = for {
      fib <- io.debug.start
      result <- fib.join
    } yield result

    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(ex) => IO.raiseError(ex)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  def testEx1(): IO[Unit] = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO.raiseError(new RuntimeException("Hi")).debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    // my answer
//    for {
//      ioaFiber <- ioa.start
//      iobFiber <- iob.start
//      ioaOutcome <- ioaFiber.join
//      iobOutcome <- iobFiber.join
//      result <- (ioaOutcome, iobOutcome) match {
//                  case (Succeeded(ioav), Succeeded(iobv)) => ioav.flatMap(av => iobv.map(bv => (av, bv)))
//                  case (Errored(ex), _) => IO.raiseError[(A, B)](new RuntimeException("first io is error"))
//                  case (_, Errored(ex)) => IO.raiseError[(A, B)](ex)
//                  case (Canceled(), Canceled()) | (_, Canceled()) | (Canceled(), _) =>
//                    IO.raiseError[(A, B)](new RuntimeException("computation is cancelled"))
//                  case (_, _) => IO.raiseError[(A, B)](new RuntimeException("unknown"))
//                }
//    } yield result

    val result = for {
      fiba <- ioa.start
      fibb <- iob.start
      resulta <- fiba.join
      resultb <- fibb.join
    } yield (resulta, resultb)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Some computation canceled."))
    }
  }

  def testEx2(): IO[Unit] = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug

    tupleIOs(firstIO, secondIO).debug.void
  }

  // 3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    // my anwser
//    for {
//      ioFiber <- io.start
//      startTime = System.currentTimeMillis().millis
//      ioOutcome <- ioFiber.join
//      endTime     = System.currentTimeMillis().millis
//      elapsedTime = endTime - startTime
//      result <- if (elapsedTime > duration) {
//                  ioFiber.cancel
//                  IO.raiseError[A](new RuntimeException("Computation Cancelled"))
//                } else {
//                  ioOutcome match {
//                    case Canceled()      => IO.raiseError[A](new RuntimeException("Computation cancelled"))
//                    case Errored(ex)     => IO.raiseError[A](ex)
//                    case Succeeded(v: A) => IO.pure[A](v)
//                  }
//                }
//    } yield result

    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start // careful - fibers can leak
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  def testEx3() = {
    val computation = IO("starting").debug >> IO.sleep(2.second) >> IO("done!").debug >> IO(42)
    timeout(computation, 1.seconds).debug.void
  }

  override def run: IO[Unit] =
    testEx3()

}
