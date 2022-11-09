package io.icednut.cats.effect.exercise.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, *}
import io.icednut.cats.effect.exercise.utils.*

import java.io.FileReader
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple:

  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open(): IO[String]  = IO(s"opening connection to $url").debug
    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open() *> IO.sleep(Int.MaxValue.seconds)).start // open connection leaked
    _ <- IO.sleep(1.second) *> fib.cancel // because IO is canceled
  } yield ()
  // problem: leaking resources

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- (conn.open() *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close().void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  // bracket pattern
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /** Exercise: read the file with bracket pattern
    *   - open a scanner
    *   - read the file line by line, every 100 millis
    *   - close the scanner
    *   - if cancelled/throws error, close the scanner
    */
  import java.util.Scanner
  import java.io.FileReader
  import java.io.File

  def readFile(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) {
      IO(scanner.nextLine()).debug >> IO.sleep(10.millis) >> readFile(scanner)
    } else {
      IO.unit
    }

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] =
    // my answer
//    openFileScanner(path).bracket { scanner =>
//      cats.Monad[IO].whileM_(IO(scanner.hasNext())) {
//        for {
//          _ <- IO(scanner.nextLine()).debug
//          _ <- IO.sleep(100.millis)
//        } yield ()
//      }
//    } { scanner =>
//      IO(s"closing file at $path").debug *> IO(scanner.close()).void
//    }

    IO(s"opening file at $path") *>
      openFileScanner(path).bracket { scanner =>
        readFile(scanner)
      } { scanner =>
        IO(s"closing file at $path").debug *> IO(scanner.close()).void
      }

  /** Resources: first-class type for resources
    */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // acquire a connection based on the file
        IO(new Connection(scanner.nextLine())).bracket { conn =>
//          conn.open().debug >> IO.sleep(Int.MaxValue.seconds)
          conn.open() >> IO.never
        }(conn => conn.close().debug.void)
      }(scanner => IO("closing file").debug >> IO(scanner.close()))
    // nesting resources are tedious

  val connectionResource = Resource.make(IO(new Connection("icednut.io")))(conn => conn.close().void)
  // at a later part of your code
  val resourceFetchUrl: IO[Unit] = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource                      = IO("some resource")
  val usingResource: String => IO[String] = str => IO(s"using the String: $str").debug
  val releaseResource: String => IO[Unit] = str => IO(s"finalizing the string: $str").debug.void

  val usingResourceWithBracket  = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  /** Exercise: read a text file with one line every 100 millis, using Resource (refactor the bracket exercise to use
    * Resource)
    */
  def resourceReadFile(path: String): IO[Unit] =
    Resource
      .make(openFileScanner(path).debug)(scanner => IO(s"closing file: $path").debug *> IO(scanner.close()).void)
      .use { scanner =>
        readFile(scanner)
      }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2.second) >> fib.cancel
  } yield ()

  // nested resources
  def connFromConfResource(path: String) =
    Resource
      .make(openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
      .flatMap { scanner =>
        Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().debug.void)
      }

  def connFromConfigResourceClean(path: String) = ???

  val openConnection =
    connFromConfResource("src/main/resources/connection.txt").use { conn =>
      conn.open() *> IO.never
    }
    // connection + file will close automatically
  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(2.second) >> fib.cancel
  } yield ()

  // finalizers to regular IOs
  val ioWithFinalizer: IO[String] = IO("some resource").debug.guarantee(IO("freeing resource").debug.void)
  val ioWithFinalizer_v2: IO[String] = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").debug).void
    case Errored(e) => IO("nothing to release").debug.void
    case Canceled() => IO("resource got canceled, releasing what's left").debug.void
  }

//  override def run = bracketReadFile(
//    "/Users/icednut/workspace/study/cats-effect-exercise/src/main/scala/io/icednut/cats/effect/exercise/part3concurrency/Fibers.scala"
//  ).void

//  override def run = resourceFetchUrl.void

//  override def run = resourceReadFile(
//    "/Users/icednut/workspace/study/cats-effect-exercise/src/main/scala/io/icednut/cats/effect/exercise/part3concurrency/Fibers.scala"
//  ).void

//  override def run = cancelReadFile(
//    "/Users/icednut/workspace/study/cats-effect-exercise/src/main/scala/io/icednut/cats/effect/exercise/part3concurrency/Fibers.scala"
//  ).void

//  override def run = openConnection

//  override def run = canceledConnection

//  override def run = ioWithFinalizer.void

  override def run = ioWithFinalizer_v2.void
