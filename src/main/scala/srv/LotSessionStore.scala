package srv

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.Logger
import srv.IOLotSessionStore.State
import srv.LotSessionStatus.Created

trait LotSessionStore {

  def all: IO[List[LotSession]]

  def created: IO[List[LotSession]]

  def active: IO[List[LotSession]]

  def closed: IO[List[LotSession]]

  def start(id: UUID)(implicit logger: Logger[IO]): IO[LotSession]

  def stop(id: UUID)(implicit logger: Logger[IO]): IO[LotSession]
}

object LotSessionStore extends BaseStore[LotSession] {
  def fromResource(name: String): IO[LotSessionStore] = {
    for {
      lotSessions <- decodeListObjFromJsonResource(name)
      (createdL, closedL) = lotSessions.partition(_.status == Created)
      created <- makeMapByUUID(createdL)
      createdR <- Ref[IO].of(created)
      activeR <- Ref[IO].of(Map[UUID, LotSession]())
      closed <- makeMapByUUID(closedL)
      closedR <- Ref[IO].of(closed)
      refState <- Ref[IO].of(State(createdR, activeR, closedR))
    } yield new IOLotSessionStore(refState)
  }
}

final case class IOLotSessionStore(state: Ref[IO, State]) extends BaseStore[LotSession] with LotSessionStore {

  def all: IO[List[LotSession]] =
    for {
      createdL <- created
      activeL <- active
      closedL <- closed
    } yield createdL ::: activeL ::: closedL

  def created: IO[List[LotSession]] = getCreated.map(_.values.toList)

  def active: IO[List[LotSession]] = getActive.map(_.values.toList)

  def closed: IO[List[LotSession]] = getClosed.map(_.values.toList)

  def start(id: UUID)(implicit logger: Logger[IO]): IO[LotSession] =
    for {
      session <- getCreated.flatMap(byId(_, id))
      res <- IO.pure(session.start)
      _ <- logger.info(s"LotSession with id $id started")
    } yield res

  def stop(id: UUID)(implicit logger: Logger[IO]): IO[LotSession] =
    for {
      session <- getClosed.flatMap(byId(_, id))
      res <- IO.pure(session.stop)
      _ <- logger.info(s"LotSession with $id stopped")
    } yield res

  private def getCreated: IO[Map[UUID, LotSession]] = state.get.flatMap(_.created.get)

  private def getActive: IO[Map[UUID, LotSession]] = state.get.flatMap(_.active.get)

  private def getClosed: IO[Map[UUID, LotSession]] = state.get.flatMap(_.closed.get)
}

object IOLotSessionStore {

  case class State(
    created: Ref[IO, Map[UUID, LotSession]],
    active: Ref[IO, Map[UUID, LotSession]],
    closed: Ref[IO, Map[UUID, LotSession]]
  )

}
