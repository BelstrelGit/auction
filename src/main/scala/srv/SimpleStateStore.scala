package srv

import java.util.UUID

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

trait SimpleStateStore[F[_], T] {
  def add(elem: T): F[Unit]

  def update(elem: T): F[Unit]

  def remove(elem: T): F[Unit]

  def all: F[List[T]]

  def getById(id: UUID): F[Option[T]]

  def byId(id: UUID): F[T]
}

object SimpleStateStore {

  def create[F[_] : Sync, T: Key](
    extractor: Extractor[F, T]
  )(implicit ds: DataSource[F]): F[SimpleStateStore[F, T]] =
    for {
      elems <- extractor.get
      state <- Sync[F].fromEither(SimpleState.create(elems))
      ref <- Ref[F].of(state)
    } yield ImplSimpleStateStore(ref)

  private case class ImplSimpleStateStore[F[_] : Sync, T: Key](
    state: Ref[F, SimpleState[T]]
  ) extends SimpleStateStore[F, T] {

    def all: F[List[T]] = state.get >>= (ss => Sync[F].delay(ss.all))

    def add(elem: T): F[Unit] = stateUpdate(_.add(elem))

    def remove(elem: T): F[Unit] = stateUpdate(_.remove(elem))

    def update(elem: T): F[Unit] = state.update(_.update(elem))

    def getById(id: UUID): F[Option[T]] = state.get >>= (s => Sync[F].delay(s.getById(id)))

    def byId(id: UUID): F[T] = Sync[F].flatMap(getById(id))(_.liftTo[F](Key.notFound[T](id)))

    private def stateUpdate(f: SimpleState[T] => Either[Throwable, SimpleState[T]]): F[Unit] =
      state.modify { s =>
        f(s) match {
          case Right(value) => value -> Sync[F].unit
          case Left(thr) => s -> Sync[F].raiseError(thr)
        }
      }

  }

}