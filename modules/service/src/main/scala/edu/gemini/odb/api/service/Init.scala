// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api.service

import edu.gemini.odb.api.{Program, Target}
import cats.implicits._

import scala.collection.immutable.TreeMap

object Init {
  val pid0: Program.Id =
    Program.Id.unsafeFromInt(0)

  val programs: List[Program] =
    List(
      Program(
        id   = pid0,
        name = Some("Observing Stars in Constellation Orion for No Particular Reason")
      )
    )

  val targets: List[Target] =
    List(
      Target(
        id       = Target.Id.unsafeFromInt(0),
        pid      = pid0,
        name     = "Betelgeuse",
        tracking = Target.Sidereal(
          ra     = "05:55:10.305",
          dec    = "07:24:25.43"
        )
      ),
      Target(
        id       = Target.Id.unsafeFromInt(1),
        pid      = pid0,
        name     = "Rigel",
        tracking = Target.Sidereal(
          ra     = "05:14:32.272",
          dec    = "-08:12:05.90"
        )
      )
    )

  def tables: OdbTables =
    OdbTables(
      TreeMap.from(programs.fproduct(_.id).map(_.swap)),
      TreeMap.from(targets.fproduct(_.id).map(_.swap))
    )

}
