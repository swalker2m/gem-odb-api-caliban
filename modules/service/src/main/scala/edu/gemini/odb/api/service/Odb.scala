// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api.service

import edu.gemini.odb.api.{OdbDao, Program, Target}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.collection.immutable.TreeMap

/**
 * A very poor-man's prototype of a database.
 */
final case class OdbTables(
  programs: TreeMap[Program.Id, Program],
  targets:  TreeMap[Target.Id, Target]
)

object Odb {

  def create[F[_]: Sync](start: OdbTables): F[OdbDao[F]] =
    Ref.of[F, OdbTables](start).map { odb =>
      new OdbDao[F] {
        def selectProgram(id: Program.Id): F[Option[Program]] =
          odb.get.map(_.programs.get(id))

        def selectTargetsForProgram(id: Program.Id): F[List[Target]] =
          odb.get.map(_.targets.values.filter(_.pid === id).toList)
      }
    }

}
