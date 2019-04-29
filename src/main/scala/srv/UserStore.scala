package srv

import java.util.UUID

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import srv.SimpleStateStore.ImplSimpleStateStore

trait UserStore[F[_]] extends SimpleStateStore[F, User, String] {

  def singIn(user: User): F[UUID]

  def singOut(user: User): F[Boolean]

  def changePassword(user: User): F[Boolean]
}

object UserStore {

  def create[F[_] : Sync](
    implicit
    ds: DataSource[F],
    key: Key[User, String],
    logger: Logger[F]): F[UserStore[F]] = {
    for {
      elems <- ds.users
      state <- Sync[F].fromEither(SimpleState.create(elems))
      ref <- Ref[F].of(state)
    } yield new ImplUserStore[F](ref)
  }

  private[srv] case class ImplUserStore[F[_] : Sync](
    override val state: Ref[F, SimpleState[User, String]],
  )(implicit key: Key[User, String], val logger: Logger[F]
  ) extends ImplSimpleStateStore[F, User, String](state) with UserStore[F] {

    def singIn(user: User): F[UUID] = byId(user.username) >>= { u =>
      if (validatePassword(user.password, u.password)) generateToken(u)
      else notFound(user.username)
    }

    def singOut(user: User): F[Boolean] = byId(user.username) >>= { u =>
      if (user.token == u.token) removeToken(u)
      else notAuthorized(user.username)
    }

    def changePassword(user: User): F[Boolean] = byId(user.username) >>= { u =>
      if (user.token == u.token) {
        super.update(u.copy(password = user.password)) <*
          logger.info(s"User [${user.username}] change password")
      }
      else notAuthorized(user.username)
    }

    override def update(user: User): F[Boolean] = byId(user.username) >>= { u =>
      if (user.token == u.token) {
        super.update(u.copy(name = user.name, age = user.age)) <*
          logger.info(s"User [${user.username}] change personal info")
      }
      else notAuthorized(user.username)
    }

    private def generateToken(user: User): F[UUID] =
      for {
        token <- Sync[F].delay(UUID.randomUUID())
        _ <- super.update(user.copy(token = Option(token)))
        _ <- logger.info(s"User [${user.username}] log in with token: $token")
      } yield token

    private def removeToken(user: User): F[Boolean] =
      super.update(user.copy(token = None)) <* logger.info(s"User [${user.username}] log out")

    private def notFound[R](id: String): F[R] = Sync[F].raiseError(UserNotFound(id))

    private def notAuthorized[R](id: String): F[R] = Sync[F].raiseError(UserNotAuthorized(id))
  }

  private def validatePassword(pass: String, correct: String): Boolean = pass == correct

}
