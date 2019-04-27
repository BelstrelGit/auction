import com.github.nscala_time.time.Imports.{DateTime, DateTimeFormat}
import io.circe.generic.extras.Configuration
import io.circe.{Decoder, Encoder, HCursor}
import org.joda.time.format.DateTimeFormatter

package object srv {

  val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

  implicit val decoderJDT: Decoder[DateTime] =
    (c: HCursor) => Decoder.decodeString.map(dateFormatter.parseDateTime)(c)

  implicit val encoderJDT: Encoder[DateTime] =
    (dt: DateTime) => Encoder.encodeString(dateFormatter.print(dt))

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

}
