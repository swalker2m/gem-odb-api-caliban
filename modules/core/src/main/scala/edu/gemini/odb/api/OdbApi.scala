// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import cats.effect.Sync
import cats.implicits._
import caliban.schema.Annotations.GQLName

object OdbApi {

  @GQLName("Program")
  final case class ProgramView[F[_]: Sync](
    id:      Program.Id,
    name:    Option[String],
    targets: F[List[Target]]
  )

  object ProgramView {
    def fromModel[F[_]: Sync](m: Program): ProgramView[F] =
      ProgramView(
        m.id,
        m.name,
        OdbModel.findTargetsForPid(m.id)
      )
  }

  final case class FindProgramArgs(id: Program.Id)

  @GQLName("Queries")
  final case class Queries[F[_]: Sync](
    program: FindProgramArgs => F[Option[ProgramView[F]]]
  )

  def findProgram[F[_]: Sync](pid: Program.Id): F[Option[ProgramView[F]]] =
    Sync[F].delay (println(s"finding ${pid.stringValue}")) *>
      OdbModel.findProgram(pid).map(_.map(ProgramView.fromModel[F]))

  def queries[F[_]: Sync]: Queries[F] =
    Queries(args => findProgram(args.id))

//  override def run(args: List[String]): IO[ExitCode] =
//    IO(println(api.render)).as(ExitCode.Success)
//    exec[IO](query).as(ExitCode.Success)

}