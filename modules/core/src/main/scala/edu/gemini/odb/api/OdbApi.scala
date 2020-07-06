// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import cats.implicits._
import caliban.CalibanError.ExecutionError
import caliban.schema.Annotations.{GQLDescription, GQLName}
import cats.{Functor, MonadError}

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
    def toView[F[_]: MonadThrowable](odb: OdbDao[F], pid: Program.Id): ProgramView[F] =
      ProgramView(
        pid,
        m.name,
        odb.selectTargetsForProgram(pid).map(_.map { case (tid, t) => t.toView(odb, tid) })
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
    def toView[F[_]: MonadThrowable](odb: OdbDao[F], tid: Target.Id): TargetView[F] =
      TargetView(
        tid,
        odb.selectProgram(m.pid).flatMap {
          case None    => implicitly[MonadThrowable[F]].raiseError(ExecutionError(s"Target ${tid.stringValue} refers to program ${m.pid.stringValue} which doesn't exist"))
          case Some(p) => implicitly[MonadThrowable[F]].pure(p.toView(odb, m.pid))
        },
        m.name,
        m.tracking
      )
  }

  final case class SelectProgramArgs(id: Program.Id)

  final case class SelectTargetArgs(id: Target.Id)


  @GQLName("Queries")
  final case class Queries[F[_]](
    program:            SelectProgramArgs => F[Option[ProgramView[F]]],
    programs:           F[List[ProgramView[F]]],
    target:             SelectTargetArgs  => F[Option[TargetView[F]]],
    targets:            F[List[TargetView[F]]],
    targetsForProgram:  SelectProgramArgs => F[List[TargetView[F]]]
  )

  def queries[F[_]: MonadThrowable](odb: OdbDao[F]): Queries[F] = {

    def toProgramView[G[_]: Functor](g: G[(Program.Id, Program)]): G[ProgramView[F]] =
      g.map { case (pid, p) => p.toView(odb, pid) }

    def toTargetView[G[_]: Functor](g: G[(Target.Id, Target)]): G[TargetView[F]] =
      g.map { case (tid, t) => t.toView(odb, tid) }

    Queries(
      program           = input => odb.selectProgram(input.id).map(_.map(_.toView(odb, input.id))),
      programs          = odb.selectPrograms.map(toProgramView(_)), // seems i have to define it this way (or toProgramView[List]) as opposed to (toProgramView) to pick up the Functor for List?
      target            = input => odb.selectTarget(input.id).map(_.map(_.toView(odb, input.id))),
      targets           = odb.selectTargets.map(toTargetView(_)),
      targetsForProgram = input => odb.selectTargetsForProgram(input.id).map(toTargetView(_))
    )
  }

  final case class ProgramCreateArgs(input: Program.CreateProgram)
  final case class TargetCreateSiderealArgs(input: Target.CreateSiderealTarget)

  @GQLName("Mutations")
  final case class Mutations[F[_]](
    createProgram:        ProgramCreateArgs        => F[ProgramView[F]],
    createSiderealTarget: TargetCreateSiderealArgs => F[TargetView[F]],
    deleteTarget:         SelectTargetArgs         => F[Option[TargetView[F]]]
  )

  def mutations[F[_]: MonadThrowable](odb: OdbDao[F]): Mutations[F] =
    Mutations(
      createProgram        =
        args => odb.createProgram(args.input).map {
          case (pid, p) => p.toView(odb, pid)
        },

      createSiderealTarget =
        args => odb.createSiderealTarget(args.input).map {
          case (tid, t) => t.toView(odb, tid)
        },

      deleteTarget         =
        args => odb.deleteTarget(args.id).map(_.map(t => t.toView(odb, args.id)))
    )

}
