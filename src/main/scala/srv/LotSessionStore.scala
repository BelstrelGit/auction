package srv

import java.util.UUID

import akka.actor.Scheduler
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import com.github.nscala_time.time.Imports._
import io.chrisdavenport.log4cats.Logger
import org.joda.time.{DateTime, Period}
import srv.LotSessionStatus.{Active, Closed, Created}

import scala.concurrent.ExecutionContext

trait LotSessionStore[F[_]] extends SimpleStateStore[F, LotSession, UUID] {

  def created: F[List[LotSession]]

  def active: F[List[LotSession]]

  def closed: F[List[LotSession]]

  def scheduleStart(id: UUID): F[UUID]

  def scheduleStartAll: F[List[UUID]]

  def makeBet(
    username: String,
    token: UUID,
    sessionId: UUID,
    amount: BigDecimal
  ): F[Bet]
}

object LotSessionStore {

  def create[F[_] : Sync : Unsafe](
    userStore: UserStore[F],
    betStore: SimpleStateStore[F, Bet, UUID]
  )(
    implicit
    ds: DataSource[F],
    actorScheduler: Scheduler,
    executionContext: ExecutionContext,
    logger: Logger[F]
  ): F[LotSessionStore[F]] = {

    def makeMapByUUID(list: List[LotSession]): F[Map[UUID, LotSession]] =
      list
        .groupBy(_.id)
        .toList
        .traverse[F, (UUID, LotSession)] {
        case (id, List(el)) => Sync[F].pure(id -> el)
        case (id, _) => Sync[F].raiseError(MultipleLotSession(id))
      }.map(_.toMap)

    for {
      implicit0(schduler: SessionScheduler[F]) <- SessionScheduler.create
      lotSessions <- ds.lotSessions
      (createdL, closedL) = lotSessions.partition(_.status == Created)
      created <- makeMapByUUID(createdL)
      active <- Sync[F].delay(Map.empty[UUID, LotSession])
      closed <- makeMapByUUID(closedL)
      state <- Ref[F].of(State(created, active, closed))
    } yield ImplLotSessionStore(state, userStore, betStore)
  }

  final case class ImplLotSessionStore[F[_] : Sync : Unsafe](
    state: Ref[F, State],
    userStore: UserStore[F],
    betStore: SimpleStateStore[F, Bet, UUID]
  )(implicit
    scheduler: SessionScheduler[F],
    logger: Logger[F]
  ) extends LotSessionStore[F] {

    def all: F[List[LotSession]] = state.get >>= (ss => Sync[F].delay(ss.all))

    def add(elem: LotSession): F[Boolean] = stateUpdate(_.add(elem))

    def remove(elem: LotSession): F[Boolean] = stateUpdate(_.remove(elem))

    def update(elem: LotSession): F[Boolean] = stateUpdate(_.update(elem))

    def getById(id: UUID): F[Option[LotSession]] = state.get >>= (s => Sync[F].delay(s.getById(id)))

    def byId(id: UUID): F[LotSession] = Sync[F].flatMap(getById(id))(_.liftTo[F](LotSessionNotFound(id)))

    def created: F[List[LotSession]] = state.get.map(_.created.values.toList)

    def active: F[List[LotSession]] = state.get.map(_.active.values.toList)

    def closed: F[List[LotSession]] = state.get.map(_.closed.values.toList)

    def scheduleStart(id: UUID): F[UUID] = {
      import com.github.nscala_time.time.RichDuration

      for {
        session <- byId(id)
        delay = new RichDuration(new Period(DateTime.now(), session.startTime).toStandardDuration).toScalaDuration

        startCB = () => Unsafe.runSync(start(id) *> Sync[F].unit)
        endCB = () => Unsafe.runSync(stop(id) *> Sync[F].unit)

        res <- scheduler.start(id, delay, session.duration, startCB, endCB)
        _ <- logger.info(s"LotSession with id $id schedule start at ${session.startTime}")
      } yield res
    }

    def scheduleStartAll: F[List[UUID]] =
      created >>= (ss => ss.withFilter(_.startTime >= DateTime.now()).map(s => scheduleStart(s.id)).sequence)

    def makeBet(
      username: String,
      token: UUID,
      sessionId: UUID,
      amount: BigDecimal
    ): F[Bet] = {

      def checkToken(user: User, token: UUID): F[Unit] =
        if (token != user.token.get) Sync[F].raiseError(UserNotAuthorized(username))
        else Sync[F].unit

      for {
        user <- userStore.byId(username)
        _ <- checkToken(user, token)
        bet <- Sync[F].delay(Bet(UUID.randomUUID(), username, amount))
        isAdded <- stateUpdate(_.addBet(sessionId, bet))
        _ <- if (isAdded) betStore.add(bet) else Sync[F].pure(false)
      } yield bet
    }


    private def start(id: UUID): F[LotSession] =
      (state.modify(_.toActive(id)) >>= Sync[F].fromEither[LotSession]) <* logger.info(s"LotSession with id $id started")

    private def stop(id: UUID): F[LotSession] =
      (state.modify(_.toClose(id)) >>= Sync[F].fromEither[LotSession]) <* logger.info(s"LotSession with $id stopped")

    private def stateUpdate(f: State => Either[Throwable, State]): F[Boolean] =
      state.modify { s =>
        f(s) match {
          case Right(value) => value -> Sync[F].pure(true)
          case Left(thr) => s -> (Sync[F].raiseError(thr): F[Boolean])
        }
      }.flatten
  }

  private[LotSessionStore] final case class State(
    created: Map[UUID, LotSession],
    active: Map[UUID, LotSession],
    closed: Map[UUID, LotSession]
  ) extends SimpleState[LotSession, UUID] {

    def all: List[LotSession] = (created ++ active ++ closed).values.toList

    def add(session: LotSession): Either[Throwable, State] = {
      val newId = session.id

      if (nonExist(newId)) Right(copy(created = created + (newId -> session)))
      else Left(MultipleLotSession(newId))
    }

    def remove(session: LotSession): Either[Throwable, State] =
      withActive(session.id, s => Right(s.copy(active = active - session.id)))

    def update(session: LotSession): Either[Throwable, State] =
      withActive(session.id, s => Right(s.copy(active = active + (session.id -> session))))

    def getById(id: UUID): Option[LotSession] = created.get(id).orElse(active.get(id)).orElse(closed.get(id))

    private[LotSessionStore] def toActive(id: UUID): (State, Either[Throwable, LotSession]) =
      created.get(id) match {
        case Some(_) =>
          val (nC, nA, nS) = transferWithChange(id, created, active)((s: LotSession) => s.copy(status = Active))
          copy(created = nC, active = nA) -> Right(nS)
        case None =>
          this -> Left(LotSessionNotFound(id))
      }

    private[LotSessionStore] def toClose(id: UUID): (State, Either[Throwable, LotSession]) =
      active.get(id) match {
        case Some(_) =>
          val (nA, nC, nS) = transferWithChange(id, active, closed)((s: LotSession) => s.copy(status = Closed))
          copy(active = nA, closed = nC) -> Right(nS)
        case None =>
          this -> Left(LotSessionNotFound(id))
      }

    private[LotSessionStore] def addBet(id: UUID, bet: Bet): Either[Throwable, State] = {
      withActive(id, { state =>
        val session = state.active(id)

        if (bet.amount > session.currPrice)
          Right(state.copy(active = active + (id -> session.copy(
            currPrice = bet.amount,
            bets = bet :: session.bets
          ))))
        else
          Left(IllegalBetAmount(bet.amount, session.currPrice))
      })
    }

    private def withActive(id: UUID, f: State => Either[Throwable, State]): Either[Throwable, State] = {
      if (isExist(id)) {
        active.get(id) match {
          case Some(_) => f(this)
          case None => Left(ChangeNonActiveLotSession(id))
        }
      } else
        Left(LotSessionNotFound(id))
    }

    private def isExist(id: UUID): Boolean = getById(id).isDefined

    private def nonExist(id: UUID): Boolean = !isExist(id)

    private def transferWithChange[K, A, B](id: K, m1: Map[K, A], m2: Map[K, B])(f: A => B): (Map[K, A], Map[K, B], B) = {
      val newB = f(m1(id))
      val newM1 = m1 - id
      val newM2 = m2 + (id -> newB)

      (newM1, newM2, newB)
    }
  }

}


