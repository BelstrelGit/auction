package srv

import java.util.UUID

import akka.actor.Scheduler
import cats.effect.Sync
import cats.implicits._
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.ExecutionContext

final class Auction[F[_] : Sync] private(
  val userStore: UserStore[F],
  val lotStore: SimpleStateStore[F, Lot, UUID],
  val betStore: SimpleStateStore[F, Bet, UUID],
  val lotSessionStore: LotSessionStore[F]
)

object Auction {

  def create[F[_] : Sync : Unsafe](
    implicit
    ds: DataSource[F],
    l: Logger[F],
    ec: ExecutionContext,
    s: Scheduler
  ): F[Auction[F]] = {
    for {
      implicit0(sessionScheduler: SessionScheduler[F]) <- SessionScheduler.create[F]

      userStore <- UserStore.create[F]
      lotStore <- SimpleStateStore.create[F, Lot, UUID]
      betStore <- SimpleStateStore.create[F, Bet, UUID]
      lotSessionStore <- LotSessionStore.createAndStart[F](userStore, betStore)

      auction <- Sync[F].pure(new Auction[F](userStore, lotStore, betStore, lotSessionStore))
    } yield auction
  }
}
