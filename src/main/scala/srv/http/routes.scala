package srv.http

import java.util.UUID

import akka.http.scaladsl.server.Directives.{parameter, _}
import akka.http.scaladsl.server.Route
import cats.effect.IO
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import io.circe.{Decoder, Encoder}
import srv.{BadRequestDataFormat, LotSessionStore, SimpleStateStore, User, UserStore}

object routes {

  object LotSessionHttp {
    def route(store: LotSessionStore[IO])(implicit logger: Logger[IO]): Route =
      pathPrefix("session") {
        get {
          path("all") {
            complete(store.all)
          } ~
            path("created") {
              complete(store.created)
            } ~
            path("active") {
              complete(store.active)
            } ~
            path("closed") {
              complete(store.closed)
            }
        } ~
          post {
            path("bet") {
              entity(as[IO[Map[String, String]]]) { ioParams =>
                complete(
                  ioParams >>= { params =>
                    parseMakeBetParams(params) match {
                      case Right((
                        username: String,
                        token: UUID,
                        sessionId: UUID,
                        amount: BigDecimal)) =>
                        store.makeBet(username, token, sessionId, amount)
                      case Left(_) =>
                        IO.raiseError(BadRequestDataFormat)
                    }
                  })
              }
            }
          }
      }

    private def parseMakeBetParams(params: Map[String, String]) = {
      val username = params.get("username")
      val token = params.get("token")
      val sessionId = params.get("sessionId")
      val amount = params.get("amount")

      if ((username >> token >> sessionId >> amount).isDefined) {
        try {
          Right((
            username.get,
            UUID.fromString(token.get),
            UUID.fromString(sessionId.get),
            BigDecimal.exact(amount.get)))
        } catch {
          case _: Exception => Left("Bad request")
        }
      }
      else
        Left("Bad request")
    }
  }

  object SimpleStoreHttp {
    def route[T: Encoder : Decoder, K: ConvertFromString](
      prefix: String,
      store: SimpleStateStore[IO, T, K])(
      implicit
      logger: Logger[IO]
    ): Route = {

      implicit val implicitS: SimpleStateStore[IO, T, K] = store

      pathPrefix(prefix) {
        get {
          all[T, K] ~ byId[T, K]
        } ~
          post {
            add[T, K]
          } ~
          put {
            update[T, K]
          } ~
          delete {
            remove[T, K]
          }
      }
    }

    private[routes] def all[T: Encoder : Decoder, K: ConvertFromString](
      implicit
      store: SimpleStateStore[IO, T, K],
      logger: Logger[IO]
    ): Route = path("all") {
      complete(store.all)
    }

    private[routes] def byId[T: Encoder : Decoder, K: ConvertFromString](
      implicit
      store: SimpleStateStore[IO, T, K],
      logger: Logger[IO]
    ): Route = parameter("id".as[K]) { id => complete(store.byId(id)) }

    private[routes] def add[T: Encoder : Decoder, K: ConvertFromString](
      implicit
      store: SimpleStateStore[IO, T, K],
      logger: Logger[IO]
    ): Route = entity(as[IO[T]]) { ioT => complete(ioT.flatMap(store.add).as("Add success")) }

    private[routes] def update[T: Encoder : Decoder, K: ConvertFromString](
      implicit
      store: SimpleStateStore[IO, T, K],
      logger: Logger[IO]
    ): Route = entity(as[IO[T]]) { ioT => complete(ioT.flatMap(store.update).as("Update success")) }

    private[routes] def remove[T: Encoder : Decoder, K: ConvertFromString](
      implicit
      store: SimpleStateStore[IO, T, K],
      logger: Logger[IO]
    ): Route = entity(as[IO[T]]) { ioT => complete(ioT.flatMap(store.remove).as("Remove success")) }
  }

  object UserStoreHttp {
    def route(store: UserStore[IO])(implicit logger: Logger[IO]): Route = {
      import SimpleStoreHttp._

      implicit val us: UserStore[IO] = store // TODO dont want make store param implicit, needs refactor

      pathPrefix("user") {
        get {
          all[User, String] ~ byId[User, String]
        } ~
          post {
            path("singIn") {
              entity(as[IO[User]]) { ioUser =>
                complete(ioUser.flatMap(store.singIn))
              }
            } ~
              path("singOut") {
                entity(as[IO[User]]) { ioUser =>
                  complete(ioUser.flatMap(store.singOut))
                }
              } ~ add[User, String]
          } ~
          put {
            path("changePassword") {
              entity(as[IO[User]]) { ioUser =>
                complete(ioUser.flatMap(store.changePassword))
              }
            } ~ update[User, String]
          } ~
          delete {
            remove[User, String]
          }
      }
    }
  }

}
