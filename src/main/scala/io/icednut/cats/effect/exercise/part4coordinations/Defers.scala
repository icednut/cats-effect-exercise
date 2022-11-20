package io.icednut.cats.effect.exercise.part4coordinations

import cats.effect.{ Deferred, Fiber, FiberIO, IO, IOApp, Outcome, Ref }
import io.icednut.cats.effect.exercise.utils.*
import cats.syntax.traverse.*

import scala.concurrent.duration.*

object Defers extends IOApp.Simple:

  // deferred is a primitive for waiting for an effect, while some other effect completes with a value
  val aDeferred: IO[Deferred[IO, Int]]    = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      meaningOfLife <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $meaningOfLife").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] crunching numbers...").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibConsumer.join
      _ <- fibProducer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"got '$part'").debug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("File download complete").debug
           else IO("downloading...").debug >> IO.sleep(500.milli) >> notifyFileComplete(contentRef) // busy wait!
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }

  // deferred works miracles for waiting
  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      _ <- signal.get // blocks until the signal is completed
      _ <- IO("[notifier] File download complete").debug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] get '$part'").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part)
      _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent)
           else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  /** Exercises:
    *   - (medium) write a small alarm notification with two simultaneous IOs
    *     - one that increments a counter every second (a clock)
    *     - one that waits for the counter to become 10, then prints a message "time's up!"
    *
    *   - (mega hard) implement racePair with Deferred.
    *     - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
    *     - start two fibers, one for each IO
    *     - on completion (with any status), each IO needs to complete that Deferred (hint: use a finalizer from the
    *       Resources lesson) (hint2: use a guarantee call to make sure the fibers complete the Deferred)
    *     - what do you do in case of cancellatin (the hardest part)?
    */

  // 1
  // my answer
  def alarmNotification(): IO[Unit] = {
    def ticking(startTime: Long, signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      currentTime = (System.currentTimeMillis() - startTime) / 1000L
      _ <- IO(s"[$currentTime second] ticking, (debug: ${currentTime % 3})").debug
      _ <- if (currentTime % 3 == 0) {
             signal.complete("time's up!") // 여기서 signal을 Unset 상태로 변경이 가능할지?
           } else {
             IO.unit
           }
      _ <- ticking(startTime, signal)
    } yield ()

    def notifyMessage(signal: Deferred[IO, String]): IO[Unit] = for {
      message <- signal.get
      _ <- IO(message).debug
    } yield ()

    for {
      signal <- Deferred[IO, String]
      fibNoti <- notifyMessage(signal).start
      fibTicking <- ticking(startTime = System.currentTimeMillis(), signal = signal).start
      _ <- fibNoti.join
      _ <- fibTicking.join
    } yield ()
  }

  def alarmNotification_v2(): IO[Unit] = {
    def ticking(startTime: Long, signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      currentTime = (System.currentTimeMillis() - startTime) / 1000L
      _ <- IO(s"[$currentTime second] ticking, (debug: ${currentTime % 3})").debug
      nextSignal <- if (currentTime % 3 == 0) {
                      signal.complete("time's up!")
                      Deferred[IO, String] >> ??? // Need Unset Deferred
                    } else {
                      IO(signal)
                    }
      _ <- ticking(startTime, nextSignal)
    } yield ()

    def notifyMessage(signal: Deferred[IO, String]): IO[Unit] = for {
      message <- signal.get
      _ <- IO(message).debug
    } yield ()

    for {
      signal <- Deferred[IO, String]
      fibNoti <- notifyMessage(signal).start
      fibTicking <- ticking(startTime = System.currentTimeMillis(), signal = signal).start
      _ <- fibNoti.join
      _ <- fibTicking.join
    } yield ()
  }

  // rockthejvm answer
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

  // 2
  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B]) // (loser fiber, winner result)
  ]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = ???

  override def run: IO[Unit] = alarmNotification()
