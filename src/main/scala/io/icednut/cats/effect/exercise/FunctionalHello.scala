package io.icednut.cats.effect.exercise

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object FunctionalHello extends IOApp.Simple {

  def program(implicit console: Console[IO]): IO[Unit] = {
    for {
      _ <- Console[IO].print("enter your name: ")
      name <- Console[IO].readLine
      _ <- Console[IO].println(s"Hello $name")
      _ <- Console[IO].errorln(s"This is error message, $name")
    } yield ()

  }

  override def run: IO[Unit] = program
}
