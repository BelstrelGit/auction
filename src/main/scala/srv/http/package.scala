package srv

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.{`application/json` => JSON}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.effect.IO
import io.circe.Encoder
import io.circe.syntax._


package object http {
  implicit def toResponseMarshaller[A: Encoder]: ToEntityMarshaller[IO[A]] = {
    Marshaller.withFixedContentType(JSON) { ioA: IO[A] =>
      HttpEntity(
        JSON,
        Source.fromFuture(ioA.map(a => ByteString(a.asJson.noSpaces)).unsafeToFuture())
      )
    }
  }
}
