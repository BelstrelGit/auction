package srv.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.effect.{IO, Sync}
import io.circe.Encoder
import srv.{LotSessionStore, SimpleStateStore, User}

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
    def route(prefix: String, store: SimpleStateStore[IO, User]): Route =
      pathPrefix(prefix) {
        (get & path("all")) {
          complete(store.all)
        }
      }
  }

}
