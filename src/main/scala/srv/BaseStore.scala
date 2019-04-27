package srv

import java.nio.file.{Files, Paths}
import java.util.UUID

import cats.effect.IO
import cats.implicits._
import io.circe.Decoder
import io.circe.parser._

abstract class BaseStore[T: Decoder, K](implicit key: Key[T,K]) {

  def decodeListObjFromJsonResource(name: String): IO[List[T]] = IO {
    decode[List[T]](
      Files.readAllBytes(
        Paths.get(getClass.getResource(name).toURI))
        .map(_.toChar).mkString): Either[Throwable, List[T]]
  }.rethrow

  def makeMapByUUID(list: List[T]): IO[Map[K, T]] =
    list
      .groupBy(Key.key[T, K])
      .toList
      .traverse {
        case (id, List(el)) => IO.pure(id -> el)
        case (id, _) => IO.raiseError(Key.multipleKey(id))
      }.map(_.toMap)

  def getById(map: Map[K, T], id: K): IO[Option[T]] = IO.pure(map.get(id))

  def byId(map: Map[K, T], id: K): IO[T] = getById(map, id).flatMap(_.liftTo[IO](Key.notFound(id)))
}
