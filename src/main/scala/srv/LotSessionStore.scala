package srv

import java.util.UUID

import akka.actor.Scheduler
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
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

  def scheduleStartAll: IO[List[UUID]]

  def start(id: UUID): IO[LotSession]

  def stop(id: UUID): IO[LotSession]
}

object LotSessionStore extends BaseStore[LotSession, UUID] {
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
      active <- IO(Map[UUID, LotSession]())
      closed <- makeMapByUUID(closedL)
      refState <- Ref[IO].of(State(created, active, closed))
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
) extends BaseStore[LotSession, UUID] with LotSessionStore {

  def all: IO[List[LotSession]] =
    for {
      createdL <- created
      activeL <- active
      closedL <- closed
    } yield createdL ::: activeL ::: closedL

  def created: IO[List[LotSession]] = state.get.map(_.created.values.toList)

  def active: IO[List[LotSession]] = state.get.map(_.active.values.toList)

  def closed: IO[List[LotSession]] = state.get.map(_.closed.values.toList)

  def scheduleStart(id: UUID): IO[UUID] = {
    import com.github.nscala_time.time.RichDuration

    for {
      c <- state.get.map(_.created)
      session <- byId(c, id)
      delay = new RichDuration(new Period(DateTime.now(), session.startTime).toStandardDuration).toScalaDuration
      _ <- logger.info(s"delay: $delay")
      res <- scheduler.start(this, id, delay)
      _ <- logger.info(s"LotSession with id $id schedule start at ${session.startTime}")
    } yield res
  }

  def scheduleStartAll: IO[List[UUID]] =
    created >>= (ss => ss.map(s => scheduleStart(s.id)).sequence)

  def start(id: UUID): IO[LotSession] =
    state.modify(_.toActive(id)).flatten <* logger.info(s"LotSession with id $id started")

  def stop(id: UUID): IO[LotSession] =
    state.modify(_.toClose(id)).flatten <* logger.info(s"LotSession with $id stopped")

}

object IOLotSessionStore {

  case class State(
    created: Map[UUID, LotSession],
    active: Map[UUID, LotSession],
    closed: Map[UUID, LotSession]
  ) {

    def transferWithChange[K, A, B](
      id: K,
      m1: Map[K, A],
      m2: Map[K, B])(
      f: A => B
    ): (Map[K, A], Map[K, B], B) = {
      val newB = f(m1(id))
      val newM1 = m1 - id
      val newM2 = m2 + (id -> newB)
      (newM1, newM2, newB)
    }

    def toActive(id: UUID): (State, IO[LotSession]) = created.get(id) match {
      case Some(_) =>
        val (nC, nA, nS) = transferWithChange(id, created, active)((s: LotSession) => s.copy(status = Active))
        copy(created = nC, active = nA) -> IO.pure(nS)
      case None => this -> IO.raiseError(LotSessionNotFound(id))
    }

    def toClose(id: UUID): (State, IO[LotSession]) = active.get(id) match {
      case Some(_) =>
        val (nA, nC, nS) = transferWithChange(id, active, closed)((s: LotSession) => s.copy(status = Closed))
        copy(active = nA, closed = nC) -> IO.pure(nS)
      case None => this -> IO.raiseError(LotSessionNotFound(id))
    }
  }

}
