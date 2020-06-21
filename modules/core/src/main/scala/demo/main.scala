// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import java.util.concurrent._
import scala.concurrent.ExecutionContext.global

import cats.effect.{ Blocker, ContextShift, ConcurrentEffect, ExitCode, IO, IOApp, Timer }
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._

import edu.gemini.odb.api.OdbService

// #server
object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
      DemoServer.stream[IO].compile.drain.as(ExitCode.Success)
}

object DemoServer {
  def stream[F[_]: ConcurrentEffect : ContextShift](implicit T: Timer[F]): Stream[F, Nothing] = {
    val blockingPool = Executors.newFixedThreadPool(4)
    val blocker      = Blocker.liftExecutorService(blockingPool)
    val odbService   = OdbService.service[F]

    val httpApp0 = (
      // Routes for static resources, ie. GraphQL Playground
      resourceService[F](ResourceService.Config("/assets", blocker)) <+>
      // Routes for the ODB GraphQL service
      OdbService.routes[F](odbService)
    ).orNotFound

    val httpApp = Logger.httpApp(logHeaders = true, logBody = false)(httpApp0)

    // Spin up the server ...
    for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain
}

// #server
