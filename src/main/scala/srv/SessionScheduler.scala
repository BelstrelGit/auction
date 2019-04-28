package srv

import java.util.UUID

import akka.actor.{Cancellable, Scheduler}
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait SessionScheduler[F[_]] {
  def start(
    sessionId: UUID,
    startDelay: FiniteDuration,
    endDelay: FiniteDuration,
    startCB: () => Unit,
    endCB: () => Unit
  ): F[UUID]

  def cancel(sessionId: UUID): F[Boolean]
}

object SessionScheduler {

  def create[F[_] : Sync](
    implicit
    akkaScheduler: Scheduler,
    ec: ExecutionContext
  ): F[SessionScheduler[F]] ={
    val init = Map.empty[UUID, Cancellable]

    Ref[F].of(State(init, init)).map(ImplSessionScheduler(_))
  }

  private final case class ImplSessionScheduler[F[_] : Sync](
    state: Ref[F, State]
  )(
    implicit
    akkaScheduler: Scheduler,
    ec: ExecutionContext
  ) extends SessionScheduler[F] {

    def start(
      sessionId: UUID,
      startDelay: FiniteDuration,
      duration: FiniteDuration,
      startCB: () => Unit,
      endCB: () => Unit
    ): F[UUID] = {

      for {
        startC <- Sync[F].delay(akkaScheduler.scheduleOnce(startDelay)(startCB()))
        _ <- state.modify(_.addStart(sessionId, startC))
        endC <- Sync[F].delay(akkaScheduler.scheduleOnce(startDelay + duration)(endCB()))
        res <- state.modify(_.addEnd(sessionId, endC))
      } yield res
    }

    def cancel(sessionId: UUID): F[Boolean] =
      state.modify(_.getAndRemove(sessionId)) >>= {
        case Left(thr) => Sync[F].raiseError(thr)
        case Right(list) =>
          Sync[F].delay(list.map(_.fold(false)(c => c.cancel())).foldLeft(false)(_ || _))
      }
  }

  private final case class State(startC: Map[UUID, Cancellable], endC: Map[UUID, Cancellable]) {

    def addStart(sessionId: UUID, cancellable: Cancellable): (State, UUID) =
      copy(startC = startC + (sessionId -> cancellable)) -> sessionId

    def addEnd(sessionId: UUID, cancellable: Cancellable): (State, UUID) =
      copy(endC = endC + (sessionId -> cancellable)) -> sessionId

    def getAndRemove(sessionId: UUID): (State, Either[Throwable, List[Option[Cancellable]]]) = {

      val sCancellable = startC.get(sessionId)
      val eCancellable = endC.get(sessionId)

      if (sCancellable.isDefined && eCancellable.isDefined)
        copy(startC = startC - sessionId) -> Right(List(sCancellable, eCancellable))
      else
        this -> Left(LotSessionNotFound(sessionId))
    }
  }

}
