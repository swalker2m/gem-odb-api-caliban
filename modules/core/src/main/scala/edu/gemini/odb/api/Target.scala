// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import atto._
import Atto._
import caliban.InputValue
import caliban.CalibanError.ExecutionError
import caliban.schema.{ArgBuilder, Schema}
import cats._
import cats.implicits._
import gem.EphemerisKey
import gem.`enum`.EphemerisKeyType

final case class Target(
  id:       Target.Id,
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

  sealed trait Tracking

//  implicit val ephemerisKeySchema: Schema[Any, EphemerisKey] =
//    Schema.stringSchema.contramap(EphemerisKey.fromString.reverseGet)
//
  final case class NonSidereal(
    key:   EphemerisKeyType,
    des:   String,
  ) extends Tracking

  final case class NonSiderealInput(
    key:   EphemerisKeyType,
    des:   String
  ) {
    def toNonSidereal: NonSidereal =
      NonSidereal(key, des)
  }

  object NonSiderealInput {

    // Performs validation on the ephemeris key des
    implicit def nonSiderealInputArgBuilder(implicit kb: ArgBuilder[EphemerisKeyType], db: ArgBuilder[String]): ArgBuilder[NonSiderealInput] = {
      case InputValue.ObjectValue(fields) =>
        for {
          kv <- fields.get("key").toRight(ExecutionError("NonSiderealInput object missing `key` field"))
          k  <- kb.build(kv)
          dv <- fields.get("des").toRight(ExecutionError("NonSiderealInput object missing `des` field"))
          d  <- db.build(dv)
          _  <- EphemerisKey.fromTypeAndDes.getOption((k, d)).toRight(ExecutionError(s"Invalid NonSiderealInput type and des combination: ${k.shortName}, $d"))
        } yield NonSiderealInput(k, d)

      case other                          =>
        Left(ExecutionError(s"Can't build a NonSiderealInput from input $other"))
    }
  }

  final case class Coordinates(
    ra:                 String,
    raMicroarcseconds:  Long,
    raHours:            BigDecimal,
    dec:                String,
    decMicroarcseconds: Long,
    decDegrees:         BigDecimal
  )

  /*
  final case class CoordinatesInput(
    ra:                 Option[String],
    raMicroarcseconds:  Option[Long],
    raHours:            Option[BigDecimal],
    dec:                Option[String],
    decMicroarcseconds: Option[Long],
    decDegrees:         Option[BigDecimal]
  )

  object CoordinatesInput {

    implicit def coordinatesInput(implicit sb: ArgBuilder[String], lb: ArgBuilder[Long], db: ArgBuilder[BigDecimal]): ArgBuilder[CoordinatesInput] = {
      case InputValue.ObjectValue(fields) =>
        for {
          ras  <- fields.get("ra")
          ral  <- fields.get("raMicroarcseconds")
          rad  <- fields.get("raHours")
          ra    = {

          }
        }
      case other                          =>
        Left(ExecutionError(s"Can't build CoordinatesInput from input $other"))
    }

  }
   */

  final case class Sidereal(
    ra:     String,
    dec:    String
  ) extends Tracking
  /*
  sealed trait EphemerisKeyType

  object EphemerisKeyType {
    case object AsteroidNew  extends EphemerisKeyType
    case object AsteroidOld  extends EphemerisKeyType
    case object Comet        extends EphemerisKeyType
    case object MajorBody    extends EphemerisKeyType
    case object UserSupplied extends EphemerisKeyType
  }

  final case class EphemerisKey(

  )
   */

}
