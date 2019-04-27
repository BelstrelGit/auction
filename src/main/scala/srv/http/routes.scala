package srv.http

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

  object SimpleStoreHttp { //TODO problem with exceptions + handler
    def route[T: Encoder : Decoder, K: ConvertFromString](prefix: String, store: SimpleStateStore[IO, T, K]): Route =
      pathPrefix(prefix) {
        get {
          path("all") {
            complete(store.all)
          } ~
            parameter("id".as[K]) { id =>
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
