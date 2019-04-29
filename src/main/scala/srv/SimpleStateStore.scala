package srv

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

trait SimpleStateStore[F[_], T, K] {
  def add(elem: T): F[Boolean]

  def update(elem: T): F[Boolean]

  def remove(elem: T): F[Boolean]

  def all: F[List[T]]

  def getById(id: K): F[Option[T]]

  def byId(id: K): F[T]
}

object SimpleStateStore {

  def create[F[_] : Sync, T, K]
  (implicit
    ds: DataSource[F],
    key: Key[T, K],
    extractor: Extractor[F, T]
  ): F[SimpleStateStore[F, T, K]] = {
    for {
      elems <- extractor.get
      state <- Sync[F].fromEither(SimpleState.create(elems))
      ref <- Ref[F].of(state)
    } yield new ImplSimpleStateStore(ref)
  }

  private[srv] class ImplSimpleStateStore[F[_] : Sync, T, K](
    val state: Ref[F, SimpleState[T, K]]
  )(implicit key: Key[T, K]
  ) extends SimpleStateStore[F, T, K] {

    def all: F[List[T]] = state.get >>= (ss => Sync[F].delay(ss.all))

    def add(elem: T): F[Boolean] = stateUpdate(_.add(elem))

    def remove(elem: T): F[Boolean] = stateUpdate(_.remove(elem))

    def update(elem: T): F[Boolean] = stateUpdate(_.update(elem))

    def getById(id: K): F[Option[T]] = state.get >>= (s => Sync[F].delay(s.getById(id)))

    def byId(id: K): F[T] = Sync[F].flatMap(getById(id))(_.liftTo[F](Key.notFound[T, K](id)))

    private def stateUpdate(f: SimpleState[T, K] => Either[Throwable, SimpleState[T, K]]): F[Boolean] =
      state.modify { s =>
        f(s) match {
          case Right(value) => value -> Sync[F].pure(true)
          case Left(thr) => s -> (Sync[F].raiseError(thr): F[Boolean])
        }
      }.flatten
  }

}