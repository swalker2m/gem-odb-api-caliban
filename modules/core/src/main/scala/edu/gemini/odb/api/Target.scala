// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import atto._
import Atto._
import caliban.CalibanError.ExecutionError
import caliban.schema.{ArgBuilder, Schema}
import cats._
import cats.implicits._

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

  case object NonSidereal extends Tracking

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
