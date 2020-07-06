// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api.service

import cats.effect.Sync
import edu.gemini.odb.api.{OdbDao, Program, Target}
import cats.implicits._

object Init {
  def initialize[F[_]: Sync]: OdbDao[F] => F[Unit] = { dao =>
    for {
      p <- dao.createProgram(
             Program.CreateProgram(
               Some("Observing Stars in Constellation Orion for No Particular Reason")
             )
           )
      _ <- dao.createSiderealTarget(
             Target.CreateSiderealTarget(
               p._1,
               "Betelgeuse",
               "05:55:10.305",
               "07:24:25.43"
             )
           )
      _ <- dao.createSiderealTarget(
             Target.CreateSiderealTarget(
               p._1,
               "Rigel",
               "05:14:32.272",
               "-08:12:05.90"
             )
           )
    } yield ()
  }

}
