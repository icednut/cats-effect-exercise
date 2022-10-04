package io.icednut.cats.effect.exercise.part2effects

import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.io.StdIn.readLine

object Effects {

  // pure function programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b
  val five                         = combine(2, 3)
  val five_v2                      = 2 + 3
  val five_v3                      = 5

  // referential trasparency = can replace an expression with its value as many times as we want without changing behavior
  // example: change a variable
  val printSomething: Unit    = println("Cats effect")
  val printSomething_v2: Unit = ()

  // example: change a variable
  var anInt                = 0
  val changingVar: Unit    = anInt += 1
  val changingVar_v2: Unit = ()

  // side effects are inevitable for useful programs

  /*
    Effect types
    Properties:
    - type signature describes the kind of calculation that will be perpormed
    - type signature describes the VALUE that will be calculated
    - when side effects are needed, effect construction is separate from effect execution
   */

  /*
    example: Option is an effect type.
    - describes a possibly absent value
    - compute a value of type A, if it exists
    - side effects are not needed
   */
  val anOption: Option[Int] = Option(42)

  /*
    example: Future is NOT an effect type.
    - describes an asynchronous computation
    - computes a value of type A, if it's successful
    - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
    example: MyIO data type from the Monads lesson
    - describes any computation that might produce side effects
    - calculate a value of type A, if its' successful
    - side effects are required for the evaluations of () => A
      - YES, the creation of MyIO does NOT produce the side effects on construction
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO { () =>
    println("I'm writing something...")
    42
  }

  /** Exercises
    *   1. An IO which returns the current time of the system 2. An IO which measures the duration of a computation 3.
    *      An IO which prints something to the console 4. An IO which reads a line (a string) from the std input
    */

  // 1
  def currentTime: MyIO[Long] =
    MyIO(() => Timestamp.valueOf(LocalDateTime.now()).getTime)

  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
//  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
//    currentTimeMillis <- currentTime
//    _ <- computation
//    endTimeMillis <- currentTime
//  } yield endTimeMillis - currentTimeMillis
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
    _ <- computation
    finishTime <- clock
  } yield finishTime - startTime

  /*
    clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

    clock.map(finishTime => finishTime - startTime) = MyIO(() => System.currentTimeMillis() - startTime)
    => clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))

    computation.flatMap(_ => clock.map(finishTime => finishTime - startTime))
    computation.flatMap(lambda) = MyIO(() => lambda(__COMP__).unsafeRun())
                                = MyIO(() => MyIO(() => System.currentTimeMillis() - startTime).unsafeRun())
                                = MyIO(() => System.currentTimeMillis_after_computation() - startTime)
    => clock.flatMap(startTime => MyIO(() => System.currentTimeMillis_after_computation() - startTime))

    clock.flatMap(startTime => MyIO(() => System.currentTimeMillis_after_computation() - startTime))
    = MyIO(() => MyIO(() => System.currentTimeMillis_after_computation() - MyIO(() => System.currentTimeMillis()).unsafeRun()).unsafeRun())
    = MyIO(() => MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis()).unsafeRun())
    = MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis_at_start())
   */

  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))

    println(test.unsafeRun())
  }

  // 3
  def printForEffect[A](something: A): MyIO[Unit] =
    MyIO(() => println(something))

  def putStrLn(line: String): MyIO[Unit] =
    MyIO(() => println(line))

  // 4
  def readLineForEffect: MyIO[String] =
    MyIO(() => readLine("input: "))

  val read: MyIO[String] = MyIO(() => StdIn.readLine("input: "))

  def testConsole(): Unit = {
    val program = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield ()

    program.unsafeRun()
  }

  def main(args: Array[String]): Unit = {
    val program = measure {
      for {
        name <- readLineForEffect
        _ <- printForEffect(s"Hello, $name")
      } yield ()
    }
    val program2 = measure {
      for {
        name <- read
        _ <- putStrLn(s"Hello, $name")
      } yield ()
    }

    program.unsafeRun()
    program2.unsafeRun()
    testTimeIO()
    testConsole()
  }
}
