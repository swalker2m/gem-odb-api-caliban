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
    targets: F[List[TargetView[F]]]
  )

  object ProgramView {
    def fromModel[F[_]: Sync](odb: OdbDao[F], m: Program): ProgramView[F] =
      ProgramView(
        m.id,
        m.name,
        odb.selectTargetsForProgram(m.id).map(_.map(TargetView.fromModel(odb, _)))
      )
  }

  @GQLName("Target")
  final case class TargetView[F[_]: Sync](
    id:       Target.Id,
    program:  F[ProgramView[F]],
    name:     String,
    tracking: Target.Tracking
  )

  object TargetView {
    def fromModel[F[_]: Sync](odb: OdbDao[F], m: Target): TargetView[F] =
      TargetView(
        m.id,
        odb.selectProgram(m.pid).map(o => ProgramView.fromModel(odb, o.get)),
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
      odb.selectProgram(pid).map(_.map(ProgramView.fromModel[F](odb, _)))

    Queries(args => findProgram(args.id))
  }

}
