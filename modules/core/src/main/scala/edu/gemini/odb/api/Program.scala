// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import atto._
import Atto._
import caliban.CalibanError.ExecutionError
import caliban.schema.{ArgBuilder, Schema}
import cats._
import cats.implicits._

final case class Program(
  name: Option[String]
)

object Program {
  val idPrefix: String =
    "program"

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

    implicit val programIdSchema: Schema[Any, Program.Id] =
      Schema.stringSchema.contramap(_.stringValue)

    implicit val programIdArgBuilder: ArgBuilder[Program.Id] =
      ArgBuilder.string.flatMap { s =>
        Program.Id.fromString(s).toRight(ExecutionError(s"Invalid program id '$s''"))
      }
  }

  final case class CreateProgram(
    name: Option[String],
  ) {

    val toProgram: Program =
      Program(name)
  }

}
