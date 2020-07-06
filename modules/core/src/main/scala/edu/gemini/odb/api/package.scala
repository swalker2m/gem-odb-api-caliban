// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb

import cats.data.ValidatedNec

package object api {

  type Result[A] = ValidatedNec[Error, A]

}
