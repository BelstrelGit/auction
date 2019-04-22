package srv.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import srv.LotSessionStore

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
