package srv

import java.util.UUID

import akka.actor.{Cancellable, Scheduler}
import cats.effect._
import cats.effect.concurrent.Ref
import srv.IOScheduler.State

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait SessionScheduler {
  def start(
    sessionStore: LotSessionStore,
    sessionId: UUID,
    delay: FiniteDuration
  )(implicit s: Scheduler, ec: ExecutionContext): IO[UUID]

  def cancel(sessionId: UUID): IO[Boolean]
}

final case class IOScheduler(state: Ref[IO, State]) extends SessionScheduler {

  def start(
    sessionStore: LotSessionStore,
    sessionId: UUID,
    delay: FiniteDuration
  )(implicit s: Scheduler, ec: ExecutionContext): IO[UUID] = {
    val cancellable = s.scheduleOnce(delay)(sessionStore.start(sessionId))
    state.modify(_.add(sessionId, IO(cancellable)))
  }

  def cancel(sessionId: UUID): IO[Boolean] =
    for {
      cancelable <- state.modify(_.getAndRemove(sessionId))
      res <- cancelable
    } yield res.cancel()

}

object IOScheduler {

  def create: IO[IOScheduler] = for {res <- Ref[IO].of(State(Map[UUID, IO[Cancellable]]()))} yield IOScheduler(res)

  final case class State(started: Map[UUID, IO[Cancellable]]) {
    def add(sessionId: UUID, cancellable: IO[Cancellable]): (State, UUID) =
      State(started = started + (sessionId -> cancellable)) -> sessionId

    def getAndRemove(sessionId: UUID): (State, IO[Cancellable]) = {

      val io = started.get(sessionId) match {
        case Some(cancellable) => cancellable
        case None => IO.raiseError(LotSessionNotFound(sessionId))
      }

      State(started = started - sessionId) -> io
    }
  }

}
