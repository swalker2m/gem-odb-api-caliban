// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

/**
 * Represents what would be database access in a real implementation.
 */
trait OdbDao[F[_]] {

  def selectProgram(id: Program.Id): F[Option[Program]]

  def selectTargetsForProgram(id: Program.Id): F[List[Target]]

}
