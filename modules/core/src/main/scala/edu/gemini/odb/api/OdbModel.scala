// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

//import gsp.math.{Coordinates, Declination, Epoch, Offset, ProperMotion, RadialVelocity, RightAscension}
//import gsp.math.syntax.int._
import cats.implicits._
import cats.effect.Sync

object OdbModel {

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
      Target.Sidereal(
        id     = Target.Id.unsafeFromInt(0),
        pid    = pid0,
        name   = "Betelgeuse",
        ra     = "05:55:10.305",
        dec    = "07:24:25.43"
      ),
      Target.Sidereal(
        id     = Target.Id.unsafeFromInt(1),
        pid    = pid0,
        name   = "Rigel",
        ra     = "05:14:32.272",
        dec    = "-08:12:05.90"
      )
/*
      Target(
        id     = Target.Id.unsafeFromInt(0),
        pid    = pid0,
        target = gem.Target(
          "Betelgeuse",
          ProperMotion(
            Coordinates(
              RightAscension.fromStringHMS.unsafeGet("05:55:10.305"),
              Declination.fromStringSignedDMS.unsafeGet("07:24:25.43")
            ),
            Epoch.J2000,
            Some(Offset(27540.µas.p, 11300.µas.q)),
            Some(RadialVelocity.fromKilometersPerSecond.unsafeGet(BigDecimal("21.844"))),
            Some(6550.µas)
          ).asRight[EphemerisKey]
        )
      ),
      Target(
        id     = Target.Id.unsafeFromInt(1),
        pid    = pid0,
        target = gem.Target(
          "Rigel",
          ProperMotion(
            Coordinates(
              RightAscension.fromStringHMS.unsafeGet("05:14:32.272"),
              Declination.fromStringSignedDMS.unsafeGet("-08:12:05.90")
            ),
            Epoch.J2000,
            Some(Offset(1310.µas.p, 500.µas.q)),
            Some(RadialVelocity.fromKilometersPerSecond.unsafeGet(BigDecimal("17.687"))),
            Some(3780.µas)
          ).asRight[EphemerisKey]
        )
      )
 */
    )

  def findProgram[F[_]: Sync](pid: Program.Id): F[Option[Program]] =
    Sync[F].delay(programs.find(_.id === pid))

  def findTargetsForPid[F[_]: Sync](pid: Program.Id): F[List[Target]] =
    Sync[F].delay(targets.filter(_.pid === pid))
}
