// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api


sealed trait Error {
  def message: String
}

object Error {

  final case class InvalidField(
    name:    String,
    input:   String,
    failure: String
  ) extends Error {

    override def message: String =
      s"Could not validate $name field value `$input`: $failure"

  }

  final case class MissingReference(
    name:  String,
    value: String
  ) extends Error {

    override def message: String =
      s"Could not find $name '$value''"

  }

}