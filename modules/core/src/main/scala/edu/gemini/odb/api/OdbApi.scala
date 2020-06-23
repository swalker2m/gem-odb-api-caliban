// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import cats.effect.Sync
import cats.implicits._
import caliban.CalibanError.ExecutionError
import caliban.schema.Annotations.GQLName

object OdbApi {

  @GQLName("Program")
  final case class ProgramView[F[_]: Sync](
    id:      Program.Id,
    name:    Option[String],
    targets: F[List[TargetView[F]]]
  )

  private implicit class ProgramOps(m: Program) {
    def toView[F[_]: Sync](odb: OdbDao[F]): ProgramView[F] =
      ProgramView(
        m.id,
        m.name,
        odb.selectTargetsForProgram(m.id).map(_.map(_.toView(odb)))
      )
  }

  @GQLName("Target")
  final case class TargetView[F[_]: Sync](
    id:       Target.Id,
    program:  F[ProgramView[F]],
    name:     String,
    tracking: Target.Tracking
  )

  private implicit class TargetOps(m: Target) {
    def toView[F[_]: Sync](odb: OdbDao[F]): TargetView[F] =
      TargetView(
        m.id,
        odb.selectProgram(m.pid).flatMap {
          case None    => Sync[F].raiseError(ExecutionError(s"Target ${m.id} refers to program ${m.pid} which doesn't exist"))
          case Some(p) => Sync[F].pure(p.toView(odb))
        },
        m.name,
        m.tracking
      )
  }

  final case class FindProgramArgs(id: Program.Id)

  @GQLName("Queries")
  final case class Queries[F[_]: Sync](
    program: FindProgramArgs => F[Option[ProgramView[F]]]
  )

  def queries[F[_]: Sync](odb: OdbDao[F]): Queries[F] = {

    def findProgram(pid: Program.Id): F[Option[ProgramView[F]]] =
      odb.selectProgram(pid).map(_.map(_.toView(odb)))

    Queries(args => findProgram(args.id))
  }

}
