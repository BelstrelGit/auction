import java.util.concurrent.TimeUnit

import cats.effect.IO
import com.github.nscala_time.time.Imports.{DateTime, DateTimeFormat}
import io.circe._
import io.circe.generic.extras.Configuration
import org.joda.time.format.DateTimeFormatter

import scala.concurrent.duration.FiniteDuration

package object srv {

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

  implicit val decoderJDT: Decoder[DateTime] =
    (c: HCursor) => Decoder.decodeString.map(dateFormatter.parseDateTime)(c)

  implicit val encoderJDT: Encoder[DateTime] =
    (dt: DateTime) => Encoder.encodeString(dateFormatter.print(dt))

  implicit final val decoderFiniteDuration: Decoder[FiniteDuration] =
    (c: HCursor) =>
      for {
        length <- c.downField("length").as[Long]
        unitS <- c.downField("unit").as[String]
        unit = TimeUnit.valueOf(unitS.toUpperCase)
      } yield FiniteDuration(length, unit)

  implicit final val encoderFiniteDuration: Encoder[FiniteDuration] =
    (a: FiniteDuration) => Json.fromJsonObject(
      JsonObject(
        "length" -> Json.fromLong(a.length),
        "unit" -> Json.fromString(a.unit.name)
      )
    )

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val IORunner: Unsafe[IO] = new Unsafe[IO] {
    override def runSync[A](effect: IO[A]): A = effect.unsafeRunSync()
  }
}
