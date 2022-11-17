package io.icednut.cats.effect.exercise.part3concurrency

import cats.effect.{IO, IOApp}
import io.icednut.cats.effect.exercise.utils._
import scala.concurrent.duration._

object CancellingIOs extends IOApp.Simple:

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */
  val chainOfIOs: IO[Int] = IO("waiting").debug >> IO.canceled >> IO(42).debug

  // uncancelable
  // example: online store, payment processor
  // payment process must NOT canceled
  val specialPaymentSystem = (
    IO("Payment running, don't cancel me...").debug >>
      IO.sleep(1.second) >>
      IO("Payment completed.").debug
  ).onCancel(IO("MEGA CANCEL OF DOOM!").debug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // same

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
    The uncancelible API is more complex and more general.
    It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
    The poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  /*
    Example: authentication service. Has two parts:
    - input password, can be cancelled, because otherwise we might block indefinitely on your input
    - verify password, CANNOT be cancelled once it's started
   */
  val inputPassword: IO[String] = IO("Input password: ").debug >> IO("(typing password)").debug >> IO.sleep(5.seconds) >> IO("RockTheJVM1!")
  val verifyPassword: String => IO[Boolean] = (pw: String) => IO("verifying...").debug >> IO.sleep(2.second) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void) // this is cancelable
      verified <- verifyPassword(pw) // this is NOT cancelable
      _ <- if (verified) IO("Authentication successful.").debug else IO("Authentication failed.").debug // this is NOT cancelable
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.second) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
    Uncancelable calls MASKS which suppress cancellation.
    Poll calls are "gaps opened" in the uncancelable region.
   */

  /**
   * Exercises
   */
  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug)
  // uncancelable will eliminate ALL cancel points

  // 2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(3.second) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug >> IO.sleep(1.second)) >>
        IO("uncancelable").debug >> IO.sleep(1.second) >>
        poll(IO("second cancelable").debug >> IO.sleep(1.second))
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("CANCELING").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run = authProgram
