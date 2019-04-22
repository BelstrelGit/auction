package srv

import java.nio.file.{Files, Paths}
import java.util.UUID

import cats.effect.IO
import cats.implicits._
import io.circe.Decoder
import io.circe.parser._

abstract class BaseStore[A: Decoder : Key] {

  def decodeListObjFromJsonResource(name: String): IO[List[A]] = IO {
    decode[List[A]](
      Files.readAllBytes(
        Paths.get(getClass.getResource(name).toURI))
        .map(_.toChar).mkString): Either[Throwable, List[A]]
  }.rethrow

  def makeMapByUUID(list: List[A]): IO[Map[UUID, A]] =
    list
      .groupBy(Key.key[A])
      .toList
      .traverse {
        case (id, List(el)) => IO.pure(id -> el)
        case (id, _) => IO.raiseError(Key.multipleKey(id))
      }.map(_.toMap)

  def getById(map: Map[UUID, A], id: UUID): IO[Option[A]] = IO.pure(map.get(id))

  def byId(map: Map[UUID, A], id: UUID): IO[A] = getById(map, id).flatMap(_.liftTo[IO](Key.notFound(id)))
}
