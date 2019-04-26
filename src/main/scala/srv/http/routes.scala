package srv.http

import java.util.UUID

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.effect.IO
import cats.implicits._
import io.circe.{Decoder, Encoder}
import srv.{LotSessionStore, SimpleStateStore}


object routes {

  object LotSessionHttp {
    def route(prefix: String, store: LotSessionStore): Route =
      pathPrefix(prefix) {
        (get & path("all")) {
          complete(store.all)
        } ~
          (get & path("created")) {
            complete(store.created)
          } ~
          (get & path("active")) {
            complete(store.active)
          } ~
          (get & path("closed")) {
            complete(store.closed)
          }
      }
  }

  object SimpleStoreHttp {
    def route[T: Encoder : Decoder](prefix: String, store: SimpleStateStore[IO, T]): Route =
      pathPrefix(prefix) {
        get {
          path("all") {
            complete(store.all)
          } ~
            parameter("id".as[UUID]) { id =>
              complete(store.byId(id))
            }
        } ~
          post {
            entity(as[IO[T]]) { ioT =>
              complete(ioT.flatMap(store.add).as("Add success"))
            }
          } ~
          put {
            entity(as[IO[T]]) { ioT =>
              complete(ioT.flatMap(store.update).as("Update success"))
            }
          } ~
          delete {
            entity(as[IO[T]]) { ioT =>
              complete(ioT.flatMap(store.remove).as("Remove success"))
            }
          }
      }
  }

}
