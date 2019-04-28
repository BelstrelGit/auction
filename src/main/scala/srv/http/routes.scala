package srv.http

import akka.http.scaladsl.server.Directives.{parameter, _}
import akka.http.scaladsl.server.Route
import cats.effect.IO
import cats.implicits._
import io.circe.{Decoder, Encoder}
import srv.{LotSessionStore, SimpleStateStore, User, UserStore}

object routes {

  object LotSessionHttp {
    def route(prefix: String, store: LotSessionStore[IO]): Route =
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
    def route[T: Encoder : Decoder, K: ConvertFromString](prefix: String, store: SimpleStateStore[IO, T, K]): Route = {

      implicit val implicitS: SimpleStateStore[IO, T, K] = store

      pathPrefix(prefix) {
        get {
          all[T, K] ~ byId[T, K]
        } ~
          post {
            add[T, K]
          } ~
          put {
            update[T,K]
          } ~
          delete {
            remove[T,K]
          }
      }
    }

    private[routes] def all[T: Encoder : Decoder, K: ConvertFromString](
      implicit store: SimpleStateStore[IO, T, K]
    ): Route = path("all") { complete(store.all) }

    private[routes] def byId[T: Encoder : Decoder, K: ConvertFromString](
      implicit store: SimpleStateStore[IO, T, K]
    ): Route = parameter("id".as[K]) { id => complete(store.byId(id)) }

    private[routes] def add[T: Encoder : Decoder, K: ConvertFromString](
      implicit store: SimpleStateStore[IO, T, K]
    ): Route = entity(as[IO[T]]) { ioT => complete(ioT.flatMap(store.add).as("Add success")) }

    private[routes] def update[T: Encoder : Decoder, K: ConvertFromString](
      implicit store: SimpleStateStore[IO, T, K]
    ): Route = entity(as[IO[T]]) { ioT => complete(ioT.flatMap(store.update).as("Update success")) }

    private[routes] def remove[T: Encoder : Decoder, K: ConvertFromString](
      implicit store: SimpleStateStore[IO, T, K]
    ): Route = entity(as[IO[T]]) { ioT => complete(ioT.flatMap(store.remove).as("Remove success")) }
  }

  object UserStoreHttp {
    def route(implicit store: UserStore[IO]): Route = {

    import SimpleStoreHttp._

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
