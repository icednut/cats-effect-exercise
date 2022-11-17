package io.icednut.cats.effect.exercise.part5polymorphic

import cats.effect.{Concurrent, Deferred, IO, IOApp}

object PolymorphicCoordination extends IOApp.Simple :

  val concurrentIO = Concurrent[IO] // given instance of Concurrent[IO], type class instance
  val aDeferred = Deferred[IO, Int] // given Concurrent[IO] in scope

  override def run: IO[Unit] = ???