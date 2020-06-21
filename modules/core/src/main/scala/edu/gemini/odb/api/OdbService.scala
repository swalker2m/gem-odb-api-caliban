// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.odb.api

import caliban.GraphQL.graphQL
import caliban.{CalibanError, GraphQL, GraphQLResponse, RootResolver}
import cats.implicits._
import cats.effect.{Async, Effect}
import io.circe._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, InvalidMessageBodyFailure, ParseFailure, QueryParamDecoder}

trait OdbService[F[_]]{
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json]
}

object OdbService {
  def routes[F[_]: Async](service: OdbService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    implicit val jsonQPDecoder: QueryParamDecoder[Json] = QueryParamDecoder[String].emap { s =>
      parser.parse(s).leftMap { case ParsingFailure(msg, _) => ParseFailure("Invalid variables", msg) }
    }

    object QueryMatcher extends QueryParamDecoderMatcher[String]("query")
    object OperationNameMatcher extends OptionalQueryParamDecoderMatcher[String]("operationName")
    object VariablesMatcher extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")

    HttpRoutes.of[F] {
      // GraphQL query is embedded in the URI query string when queried via GET
      case GET -> Root / "odb" :?  QueryMatcher(query) +& OperationNameMatcher(op) +& VariablesMatcher(vars0) =>
        vars0.sequence.fold(
          errors => BadRequest(errors.map(_.sanitized).mkString_("", ",", "")),
          vars =>
            for {
              result <- service.runQuery(op, vars, query)
              resp   <- Ok(result)
            } yield resp
          )

      // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "odb" =>
        for {
          body   <- req.as[Json]
          obj    <- body.asObject.liftTo[F](InvalidMessageBodyFailure("Invalid GraphQL query"))
          query  <- obj("query").flatMap(_.asString).liftTo[F](InvalidMessageBodyFailure("Missing query field"))
          op     =  obj("operationName").flatMap(_.asString)
          vars   =  obj("variables")
          result <- service.runQuery(op, vars, query)
          resp   <- Ok(result)
        } yield resp
    }
  }

  import caliban.interop.cats.implicits._

  implicit val runtime: zio.Runtime[zio.ZEnv] =
    zio.Runtime.default

  def api[F[_]: Effect]: GraphQL[Any] =
    graphQL(RootResolver(OdbApi.queries[F]))

  def service[F[_]](implicit F: Effect[F]): OdbService[F] =
    (op: Option[String], vars: Option[Json], query: String) =>
      for {
        _ <- F.delay(println("hi: " + vars + ", " + query))
        i <- api[F].interpreterAsync[F]
        r <- i.executeAsync[F](
          query,
          operationName = op
        )
      } yield Encoder[GraphQLResponse[CalibanError]].apply(r)
}