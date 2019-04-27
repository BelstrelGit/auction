package srv

import java.nio.file.{Files, Paths}

import cats.effect.Sync
import cats.implicits._
import io.circe.Decoder
import io.circe.parser.decode

trait DataSource[F[_]] {
  def users: F[List[User]]

  def lots: F[List[Lot]]

  def lotSessions: F[List[LotSession]]

  def bets: F[List[Bet]]
}

object DataSource {
  def file[F[_] : Sync]: F[DataSource[F]] = Sync[F].delay(new FileDataSource[F])

  private final class FileDataSource[F[_] : Sync] extends DataSource[F] {
    def users: F[List[User]] = decodeListObjFromJsonResource[User]("/data/users.json")

    def lots: F[List[Lot]] = decodeListObjFromJsonResource[Lot]("/data/lots.json")

    def lotSessions: F[List[LotSession]] = decodeListObjFromJsonResource[LotSession]("/data/sessions.json")

    def bets: F[List[Bet]] = decodeListObjFromJsonResource[Bet]("/data/bets.json")

    private def decodeListObjFromJsonResource[T: Decoder](name: String): F[List[T]] = Sync[F].delay {
      decode[List[T]](
        Files.readAllBytes(
          Paths.get(getClass.getResource(name).toURI))
          .map(_.toChar).mkString): Either[Throwable, List[T]]
    }.rethrow
  }

}


