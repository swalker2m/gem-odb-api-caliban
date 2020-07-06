// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import gem.EphemerisKey
import gem.`enum`.EphemerisKeyType
import gsp.math
import atto._
import Atto._
import caliban.CalibanError.ExecutionError
import caliban.schema.{ArgBuilder, Schema}
import cats._
import cats.implicits._

final case class Target(
  pid:      Program.Id,
  name:     String,
  tracking: Target.Tracking
)

object Target {
  val idPrefix: String =
    "target"

  abstract case class Id private (index: Int) extends ObjectId {
    assert(index >= 0, s"ids must have positive indices, not: $index")

    def prefix: String =
      idPrefix
  }

  object Id {
    def fromInt(i: Int): Option[Id] =
      if (i >= 0) Some(new Id(i) {}) else None

    def unsafeFromInt(i: Int): Id =
      fromInt(i).get

    val parser: Parser[Id] =
      ObjectId.parser(idPrefix)(i => new Id(i) {})

    def fromString(s: String): Option[Id] =
      parser.parseOnly(s).option

    def unsafeFromString(s: String): Id =
      fromString(s).get

    implicit val OrderId: Order[Id] =
      Order.by(_.index)

    implicit val targetIdSchema: Schema[Any, Target.Id] =
      Schema.stringSchema.contramap(_.stringValue)

    implicit val targetIdArgBuilder: ArgBuilder[Target.Id] =
      ArgBuilder.string.flatMap { s =>
        Target.Id.fromString(s).toRight(ExecutionError(s"Invalid target id '$s''"))
      }

  }

  object parse {

    def ephemerisKey(fieldName: String, key: gem.`enum`.EphemerisKeyType, input: String): Result[gem.EphemerisKey] =
      EphemerisKey
        .fromTypeAndDes
        .getOption((key, input))
        .toValidNec(
          Error.InvalidField(fieldName, input, s"Invalid description for ephemeris key type `${key.shortName}`")
        )

    def ra(fieldName: String, input: String): Result[math.RightAscension] =
      math.RightAscension
        .fromStringHMS
        .getOption(input)
        .toValidNec(
          Error.InvalidField(fieldName, input, "Expected right ascension in format HH:MM:SS.SSS")
        )

    def dec(fieldName: String, input: String): Result[math.Declination] =
      math.Declination
        .fromStringSignedDMS
        .getOption(input)
        .toValidNec(
          Error.InvalidField(fieldName, input, "Expected declination in format [+/-]DD:MM:SS:SS.SSS")
        )

  }

  sealed trait Tracking

  final case class NonSidereal(
    key:   EphemerisKeyType,
    des:   String,
  ) extends Tracking

  object NonSidereal {
    def fromEphemerisKey(k: gem.EphemerisKey): NonSidereal =
      NonSidereal(
        k.keyType,
        k.des
      )
  }

  final case class CreateNonSiderealTarget(
    pid:  Program.Id,
    name: String,
    key:  EphemerisKeyType,
    des:  String
  ) {

    val toEphemerisKey: Result[gem.EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toTarget: Result[Target] =
      toEphemerisKey.map { k =>
        Target(
          pid,
          name,
          NonSidereal.fromEphemerisKey(k)
        )
      }
  }

  final case class Sidereal(
    ra:     String,
    dec:    String
  ) extends Tracking

  object Sidereal {
    def fromProperMotion(p: math.ProperMotion): Sidereal =
      Sidereal(
        math.RightAscension.fromStringHMS.reverseGet(p.baseCoordinates.ra),
        math.Declination.fromStringSignedDMS.reverseGet(p.baseCoordinates.dec)
      )
  }

  final case class CreateSiderealTarget(
    pid:  Program.Id,
    name: String,
    ra:   String,
    dec:  String
  ) {

    val toProperMotion: Result[math.ProperMotion] =
      (parse.ra("ra", ra),
       parse.dec("dec", dec)
      ).mapN(
        (ra, dec) =>
          math.ProperMotion(
            math.Coordinates(ra, dec), math.Epoch.J2000, None, None, None
          )
      )

    val toTarget: Result[Target] =
      toProperMotion.map { pm =>
        Target(
          pid,
          name,
          Sidereal.fromProperMotion(pm)
        )
      }

  }

}
