package srv

import java.util.UUID

import cats.effect.{IO, Sync}

import scala.concurrent.duration.FiniteDuration
import com.github.nscala_time.time.Imports._
import io.circe.derivation.annotations.JsonCodec
import io.circe.{Decoder, Encoder}

@JsonCodec
final case class User(
  username: String,
  password: String,
  name: String,
  age: Int,
  token: Option[UUID] = None
)

object User {

  implicit val key: Key[User, String] = new Key[User, String] {
    def key(el: User): String = el.username

    def notFound(id: String): Throwable = UserNotFound(id)

    def multipleKey(id: String): Throwable = MultipleUser(id)
  }
}

@JsonCodec
final case class LotSession(
  id: UUID,
  lotId: UUID,
  currPrice: BigDecimal,
  bets: List[Bet],
  startTime: DateTime,
  duration: FiniteDuration,
  status: LotSessionStatus
)

object LotSession {
  implicit val key: Key[LotSession, UUID] = new Key[LotSession, UUID] {
    def key(el: LotSession): UUID = el.id

    def notFound(id: UUID): Throwable = LotSessionNotFound(id)

    def multipleKey(id: UUID): Throwable = MultipleLotSession(id)
  }
}

sealed trait LotSessionStatus

object LotSessionStatus {

  import io.circe.generic.extras.semiauto._

  case object Created extends LotSessionStatus

  case object Active extends LotSessionStatus

  case object Closed extends LotSessionStatus

  implicit val decoder: Decoder[LotSessionStatus] = deriveEnumerationDecoder[LotSessionStatus]
  implicit val encoder: Encoder[LotSessionStatus] = deriveEnumerationEncoder[LotSessionStatus]
}

@JsonCodec
final case class Lot(
  id: UUID,
  name: String,
  startPrice: BigDecimal,
  description: String
)

object Lot {
  implicit val key: Key[Lot, UUID] = new Key[Lot, UUID] {
    def key(el: Lot): UUID = el.id

    def notFound(id: UUID): Throwable = LotNotFound(id)

    def multipleKey(id: UUID): Throwable = MultipleLot(id)
  }

  implicit def extractor[F[_] : Sync]: Extractor[F, Lot] = (ds: DataSource[F]) => ds.lots
}

@JsonCodec
final case class Bet(
  id: UUID,
  username: String,
  amount: BigDecimal
)

object Bet {

  implicit val key: Key[Bet, UUID] = new Key[Bet, UUID] {
    def key(el: Bet): UUID = el.id

    def notFound(id: UUID): Throwable = BetNotFound(id)

    def multipleKey(id: UUID): Throwable = MultipleBet(id)
  }

  implicit def extractor[F[_] : Sync]: Extractor[F, Bet] = (ds: DataSource[F]) => ds.bets
}

abstract class StacklessException(message: String) extends Exception(message, null, false, false)

final case class UserNotFound(id: String) extends StacklessException(s"User with username $id not found")
final case class MultipleUser(id: String) extends StacklessException(s"Multiple User with username $id")
final case class UserNotAuthorized(id: String) extends StacklessException(s"User with username $id is not authorized")
case object IllegalCredentials extends StacklessException("Wrong login/password")

final case class LotSessionNotFound(id: UUID) extends StacklessException(s"LotSession with id $id not found")
final case class MultipleLotSession(id: UUID) extends StacklessException(s"Multiple LotSession with id $id")
final case class ChangeNonActiveLotSession(id: UUID) extends StacklessException(s"Attempt to change non-active session $id")

final case class LotNotFound(id: UUID) extends StacklessException(s"Lot with id $id not found")
final case class MultipleLot(id: UUID) extends StacklessException(s"Multiple Lot with id $id")

final case class BetNotFound(id: UUID) extends StacklessException(s"Bet with id $id not found")
final case class MultipleBet(id: UUID) extends StacklessException(s"Multiple Bet with id $id")
final case class IllegalBetAmount(amount: BigDecimal, min: BigDecimal) extends StacklessException(s"Bet amount must bo more that $min (current is $amount)")

case object BadRequestDataFormat extends StacklessException("Illegal input data format")