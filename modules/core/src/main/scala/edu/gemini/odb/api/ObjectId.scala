// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import atto.Atto.{char, int, letter, stringOf}
import atto._
import Atto._

import cats.implicits._

/**
 *
 */
trait ObjectId {
  def prefix: String
  def index:  Int

  def stringValue: String =
    s"$prefix${ObjectId.sep}$index"
}

object ObjectId {
  val sep: Char =
    '-'
  val parts: Parser[(String, Int)] =
    for {
      p <- stringOf(letter) <~ char(sep)
      i <- int.filter(n => n >= 0 && n <= Int.MaxValue).namedOpaque("uint")
    } yield (p, i)

  def parser[A](prefix: String)(f: Int => A): Parser[A] =
    parts.collect { case (p, i) if p === prefix => i }.map(f)
}
