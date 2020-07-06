// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api.service

import edu.gemini.odb.api.{Error, OdbDao, Program, Result, Target}
import caliban.CalibanError.ExecutionError
import cats.data.Validated
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import monocle.Lens
import monocle.function.At

import scala.collection.immutable.{SortedMap, TreeMap}


object Odb {

  final case class Database(
    programCounter: Int,
    programs:       SortedMap[Program.Id, Program],
    targetCounter:  Int,
    targets:        SortedMap[Target.Id, Target]
  )

  object Database {
    val empty: Database =
      Database(
        0,
        TreeMap.empty,
        0,
        TreeMap.empty
      )

    val programs: Lens[Database, SortedMap[Program.Id, Program]] =
      Lens[Database, SortedMap[Program.Id, Program]](_.programs)(b => a => a.copy(programs = b))

    def program(pid: Program.Id): Lens[Database, Option[Program]] =
      programs.composeLens(At.at(pid))

    val targets: Lens[Database, SortedMap[Target.Id, Target]] =
      Lens[Database, SortedMap[Target.Id, Target]](_.targets)(b => a => a.copy(targets = b))

    def target(tid: Target.Id): Lens[Database, Option[Target]] =
      targets.composeLens(At.at(tid))
  }

  def create[F[_]: Sync]: F[OdbDao[F]] =
    Ref.of[F, Database](Database.empty).map { odb =>

      def get[B](l: Lens[Database, B]): F[B] =
        odb.get.map(t => l.get(t))

      new OdbDao[F] {
        override val selectPrograms: F[List[(Program.Id, Program)]] =
          get(Database.programs).map(_.toList)

        override def createProgram(input: Program.CreateProgram): F[(Program.Id, Program)] =
          odb.modify { o =>
            val res = Program.Id.unsafeFromInt(o.programCounter) -> input.toProgram
            (
              o.copy(
                programCounter = o.programCounter + 1,
                programs       = o.programs + res
              ),
              res
            )
          }

        override def selectProgram(pid: Program.Id): F[Option[Program]] =
          get(Database.program(pid))

        override val selectTargets: F[List[(Target.Id, Target)]] =
          get(Database.targets).map(_.toList)

        override def selectTarget(tid: Target.Id): F[Option[Target]] =
          get(Database.target(tid))

        override def selectTargetsForProgram(id: Program.Id): F[List[(Target.Id, Target)]] =
          selectTargets.map(_.filter(_._2.pid === id))

        override def createSiderealTarget(input: Target.CreateSiderealTarget): F[(Target.Id, Target)] = {
          def validateInput(db: Database): Result[Target] =
            (
              db.programs.get(input.pid).toValidNec(Error.MissingReference("pid", input.pid.stringValue)),
              input.toTarget
            ).mapN((_, t) => t)

          val res = odb.modify { db =>
            validateInput(db) match {
              case Validated.Invalid(e) =>
                (
                  db,
                  Left(ExecutionError(e.toList.map(_.message).intercalate("\n")))
                )

              case Validated.Valid(t)   =>
                val tid = Target.Id.unsafeFromInt(db.targetCounter)
                (
                  db.copy(
                    targetCounter = db.targetCounter + 1,
                    targets       = db.targets + (tid -> t)
                  ),
                  Right(tid -> t)
                )
            }
          }

          res.flatMap {
            case Left(e)      =>
              Sync[F].raiseError[(Target.Id, Target)](e)
            case Right(tup)   =>
              tup.pure[F]
          }
        }

        override def deleteTarget(id: Target.Id): F[Option[Target]] =
          odb.modify { o =>
            (Database.targets.modify(_ - id)(o), o.targets.get(id))
          }
      }
    }

}
