package srv

import java.util.UUID

import akka.actor.Scheduler
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.Logger
import org.joda.time.{DateTime, Period}
import srv.IOLotSessionStore.State
import srv.LotSessionStatus.{Active, Closed, Created}

import scala.concurrent.ExecutionContext

trait LotSessionStore {

  def all: IO[List[LotSession]]

  def created: IO[List[LotSession]]

  def active: IO[List[LotSession]]

  def closed: IO[List[LotSession]]

  def scheduleStart(id: UUID): IO[UUID]

  def start(id: UUID): IO[LotSession]

  def stop(id: UUID): IO[LotSession]
}

object LotSessionStore extends BaseStore[LotSession] {
  def fromResource(
    name: String
  )(implicit
    scheduler: SessionScheduler,
    actorScheduler: Scheduler,
    executionContext: ExecutionContext,
    logger: Logger[IO]): IO[LotSessionStore] = {
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

final case class IOLotSessionStore(
  state: Ref[IO, State]
)(implicit
  scheduler: SessionScheduler,
  actorScheduler: Scheduler,
  context: ExecutionContext,
  logger: Logger[IO]
) extends BaseStore[LotSession] with LotSessionStore {

  def all: IO[List[LotSession]] =
    for {
      createdL <- created
      activeL <- active
      closedL <- closed
    } yield createdL ::: activeL ::: closedL

  def created: IO[List[LotSession]] = getCreated.map(_.values.toList)

  def active: IO[List[LotSession]] = getActive.map(_.values.toList)

  def closed: IO[List[LotSession]] = getClosed.map(_.values.toList)

  def scheduleStart(id: UUID): IO[UUID] = {
    import com.github.nscala_time.time.RichDuration

    for {
      session <- getCreated.flatMap(byId(_, id))
      delay = new RichDuration(new Period(DateTime.now(), session.startTime).toStandardDuration).toScalaDuration
      res <- scheduler.start(this, id, delay)
      _ <- logger.info(s"LotSession with id $id schedule start at ${session.startTime}")
    } yield res
  }

  def start(id: UUID): IO[LotSession] =
    for {
      session <- getCreated.flatMap(byId(_, id))
      res <- IO.pure(session.copy(status = Active))
      _ <- logger.info(s"LotSession with id $id started")
    } yield res

  def stop(id: UUID): IO[LotSession] =
    for {
      session <- getClosed.flatMap(byId(_, id))
      res <- IO.pure(session.copy(status = Closed))
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
  ) {
    def toActive(id: UUID): IO[Unit] =
      for {
        c <- created.get
        a <- active.get
      } yield ???
  }

}
