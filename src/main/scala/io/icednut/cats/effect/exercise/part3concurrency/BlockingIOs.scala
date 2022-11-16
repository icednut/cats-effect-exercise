package io.icednut.cats.effect.exercise.part3concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*
import io.icednut.cats.effect.exercise.utils.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object BlockingIOs extends IOApp.Simple:

  // really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  } // will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  /**
   * SEMENTIC BLOCKING means that the threads themselves that cats effect manages are not blocked.
   * So no thread actually does the sleeping after one second, but rather a thread will be scheduled after one second to continue the computation
   * which is why these sleeps with the log lines printed attached to different threads.
   *
   * So in semantic blocking this effect looks like it's sleeping for one second, but no actual thread is blocked for that amount of time.
   * And so in order to implement semantic blocking this effects will have to yield control over the thread that it currently runs.
   */
  val someSleeps = for {
    _ <- IO.sleep(1.second).debug // SEMANTIC BLOCKING
    _ <- IO.sleep(1.second).debug
  } yield ()

  /**
   * yielding
   *
   * There is a way to control the way that you yield control over the thread so that's a little bit of a words out there.
   * So you can control how you yield the thread that you're currently running on.
   */
  val iosOnManyThreads = for {
    _ <- IO("first").debug
//    _ <- IO.cede // a signal to yield control over the thread - equivalent to IO.shift
    _ <- IO.sleep(3.second) >> IO("second").debug // the rest of this effect may run on another thread (not necessarily)
//    _ <- IO.cede
    _ <- IO.sleep(3.second) >> IO("third").debug
  } yield ()

//  val thousandCedes = (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val thousandCedes = (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
  // Cats Effect 에서 메인 안에 있는 스레드풀은 사용하지 않으면 강제로 종료 시킴
  /**
   * Non-daemon threads currently preventing JVM termination: - 29: Thread[pool-1-thread-5,5,main]
   *  -  - 32: Thread[pool-1-thread-8,5,main]
   *  -  - 26: Thread[pool-1-thread-2,5,main]
   *  -  - 30: Thread[pool-1-thread-6,5,main]
   *  -  - 28: Thread[pool-1-thread-4,5,main]
   *  -  - 34: Thread[DestroyJavaVM,5,main]
   *  -  - 24: Thread[pool-1-thread-1,5,main]
   *  -  - 27: Thread[pool-1-thread-3,5,main]
   *  -  - 31: Thread[pool-1-thread-7,5,main]
   */

    def testThousandEffectsSwitch() = {
      val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
      (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
    }

  /*
    - blocking calls & IO.sleep and yield control over the calling thread automatic
   */

//  override def run = someSleeps.void
//  override def run = aBlockingIO.debug.void
//  override def run = iosOnManyThreads.void
//  override def run = thousandCedes.void
  override def run = testThousandEffectsSwitch() .void
