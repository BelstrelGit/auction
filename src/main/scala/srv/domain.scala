package srv

import java.util.UUID

import cats.effect.IO
import com.github.nscala_time.time.Imports._
import io.circe.derivation.annotations.JsonCodec
import io.circe.{Decoder, Encoder}
import srv.LotSessionStatus.{Active, Closed, Created}

@JsonCodec
final case class User(
  id: UUID,
  name: String
)

object User {

  implicit val key: Key[User, String] = new Key[User, String] {
    def key(el: User): String = el.name

    def notFound(id: String): Throwable = UserNotFound(id)

    def multipleKey(id: String): Throwable = MultipleUser(id)
  }

  val extractor: Extractor[IO, User] = (ds: DataSource[IO]) => ds.users
}

@JsonCodec
final case class LotSession(
  id: UUID,
  lotId: UUID,
  currPrice: BigDecimal,
  bets: List[Bet],
  startTime: DateTime,
  status: LotSessionStatus
) {

  def makeBet(bet: Bet): Either[String, LotSession] = status match {
    case Active =>
      if (bet.amount > currPrice)
        Right(copy(currPrice = bet.amount, bets = bet :: bets))
      else
        Left(s"Bet must be more then $currPrice (your bet is ${bet.amount}).")
    case Created =>
      Left(s"Session is not started yet. Make bet after $startTime.")
    case Closed =>
      Left(s"Session is closed.")
  }
}

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

  val extractor: Extractor[IO, Lot] = (ds: DataSource[IO]) => ds.lots
}

@JsonCodec
final case class Bet(
  id: UUID,
  userId: UUID,
  amount: BigDecimal
)

object Bet {
  implicit val key: Key[Bet, UUID] = new Key[Bet, UUID] {
    def key(el: Bet): UUID = el.id

    def notFound(id: UUID): Throwable = BetNotFound(id)

    def multipleKey(id: UUID): Throwable = MultipleBet(id)
  }

  val extractor: Extractor[IO, Bet] = (ds: DataSource[IO]) => ds.bets
}

abstract class StacklessException(message: String) extends Exception(message, null, false, false)

final case class UserNotFound(id: String) extends StacklessException(s"User with id $id not found")

final case class MultipleUser(id: String) extends StacklessException(s"Multiple User with id $id")

final case class LotSessionNotFound(id: UUID) extends StacklessException(s"LotSession with id $id not found")

final case class MultipleLotSession(id: UUID) extends StacklessException(s"Multiple LotSession with id $id")

final case class LotNotFound(id: UUID) extends StacklessException(s"Lot with id $id not found")

final case class MultipleLot(id: UUID) extends StacklessException(s"Multiple Lot with id $id")

final case class BetNotFound(id: UUID) extends StacklessException(s"Bet with id $id not found")

final case class MultipleBet(id: UUID) extends StacklessException(s"Multiple Bet with id $id")
