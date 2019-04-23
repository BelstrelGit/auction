package srv

import java.util.UUID

import com.github.nscala_time.time.Imports._
import io.circe.derivation.annotations.JsonCodec
import io.circe.{Decoder, Encoder}
import srv.LotSessionStatus.{Active, Closed, Created}

@JsonCodec
final case class User(
  id: UUID,
  name: String
)

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
  implicit val key: Key[LotSession] = new Key[LotSession] {
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

@JsonCodec
final case class Bet(
  id: UUID,
  userId: UUID,
  amount: BigDecimal
)

abstract class StacklessException(message: String) extends Exception(message, null, false, false)

final case class LotSessionNotFound(id: UUID) extends StacklessException(s"LotSession with id $id not found")

final case class MultipleLotSession(id: UUID) extends StacklessException(s"Multiple LotSession with id $id")
