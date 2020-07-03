// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import cats.implicits._
import caliban.CalibanError.ExecutionError
import caliban.schema.Annotations.{GQLDescription, GQLName}
import cats.MonadError

object OdbApi {

  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  @GQLName("Program")
  @GQLDescription("A single science program")
  final case class ProgramView[F[_]](
    id:      Program.Id,
    name:    Option[String],
    targets: F[List[TargetView[F]]]
  )

  private implicit class ProgramOps(m: Program) {
    def toView[F[_]: MonadThrowable](odb: OdbDao[F]): ProgramView[F] =
      ProgramView(
        m.id,
        m.name,
        odb.selectTargetsForProgram(m.id).map(_.map(_.toView(odb)))
      )
  }

  @GQLName("Target")
  @GQLDescription("Targets have a name an a 'tracking' component which describes how to find their location")
  final case class TargetView[F[_]](
    id:       Target.Id,
    program:  F[ProgramView[F]],
    name:     String,
    tracking: Target.Tracking
  )

  private implicit class TargetOps(m: Target) {
    def toView[F[_]: MonadThrowable](odb: OdbDao[F]): TargetView[F] =
      TargetView(
        m.id,
        odb.selectProgram(m.pid).flatMap {
          case None    => implicitly[MonadThrowable[F]].raiseError(ExecutionError(s"Target ${m.id} refers to program ${m.pid} which doesn't exist"))
          case Some(p) => implicitly[MonadThrowable[F]].pure(p.toView(odb))
        },
        m.name,
        m.tracking
      )
  }

  final case class FindProgramArgs(id: Program.Id)

  @GQLName("Queries")
  final case class Queries[F[_]](
    program: FindProgramArgs => F[Option[ProgramView[F]]]
  )

  def queries[F[_]: MonadThrowable](odb: OdbDao[F]): Queries[F] = {

    def findProgram(pid: Program.Id): F[Option[ProgramView[F]]] =
      odb.selectProgram(pid).map(_.map(_.toView(odb)))

    Queries(args => findProgram(args.id))
  }

}
