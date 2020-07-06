// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

/**
 * Represents what would be database access in a real implementation.
 */
trait OdbDao[F[_]] {

  def selectPrograms: F[List[(Program.Id, Program)]]

  def selectProgram(id: Program.Id): F[Option[Program]]

  def createProgram(input: Program.CreateProgram): F[(Program.Id, Program)]

  def selectTargets: F[List[(Target.Id, Target)]]

  def selectTarget(id: Target.Id): F[Option[Target]]

  def selectTargetsForProgram(id: Program.Id): F[List[(Target.Id, Target)]]

  def createSiderealTarget(input: Target.CreateSiderealTarget): F[(Target.Id, Target)]

  def deleteTarget(id: Target.Id): F[Option[Target]]
}
