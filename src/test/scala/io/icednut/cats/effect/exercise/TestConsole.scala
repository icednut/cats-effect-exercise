package io.icednut.cats.effect.exercise

import cats.Show
import cats.effect.std.Console
import cats.effect.{Ref, Sync}
import cats.syntax.all._

import java.nio.charset.Charset

class TestConsole[F[_] : Sync](
  standardOutRef: Ref[F, Vector[String]],
  errorOutRef: Ref[F, Vector[String]],
  inputRef: Ref[F, Vector[String]]
) extends Console[F] {

  override def readLineWithCharset(charset: Charset): F[String] = for {
    v <- inputRef.get
  } yield v.mkString

  override def print[A](a: A)(implicit S: Show[A]): F[Unit] = for {
    _ <- standardOutRef.modify { outputs => (outputs :+ a.toString, a.toString) }
  } yield ()

  override def println[A](a: A)(implicit S: Show[A]): F[Unit] = for {
    _ <- standardOutRef.modify { outputs => (outputs :+ (a.toString + "\n"), a.toString) }
  } yield ()

  override def error[A](a: A)(implicit S: Show[A]): F[Unit] = for {
    _ <- errorOutRef.modify { outputs => (outputs :+ a.toString, a.toString) }
  } yield ()

  override def errorln[A](a: A)(implicit S: Show[A]): F[Unit] = for {
    _ <- errorOutRef.modify { outputs => (outputs :+ (a.toString + "\n"), a.toString) }
  } yield ()

  def feedLines(message: String): F[Unit] = for {
    _ <- inputRef.set(Vector(message))
  } yield ()

  def output: F[Vector[String]] = for {
    outputs <- standardOutRef.get
  } yield outputs

  def errorOutput: F[Vector[String]] = for {
    outputs <- errorOutRef.get
  } yield outputs
}

object TestConsole {

  def apply[F[_] : Sync]: TestConsole[F] = new TestConsole[F](
    standardOutRef = Ref.unsafe[F, Vector[String]](Vector[String]()),
    errorOutRef = Ref.unsafe[F, Vector[String]](Vector[String]()),
    inputRef = Ref.unsafe[F, Vector[String]](Vector[String]())
  )
}
