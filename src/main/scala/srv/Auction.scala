package srv

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import cats.effect.{ExitCode, IO, IOApp}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import srv.http.LotSessionHttp

import scala.concurrent.ExecutionContext

object Auction extends IOApp {

  def system: IO[ActorSystem] = IO(ActorSystem("srv"))

  def runServer(route: Route)(implicit system: ActorSystem, conf: HttpConfig, logger: Logger[IO]): IO[Unit] =
    for {
      binding <- IO.fromFuture(IO {
        implicit val mat: ActorMaterializer = ActorMaterializer()
        Http().bindAndHandle(route, conf.ip, conf.port)
      })
      res <- logger.info(binding.toString)
    } yield res

  def httpConfiguration: IO[HttpConfig] = IO(HttpConfig())

  def run(args: List[String]): IO[ExitCode] =
    for {
      implicit0(s: ActorSystem) <- system
      implicit0(c: HttpConfig) <- httpConfiguration
      implicit0(l: Logger[IO]) <- Slf4jLogger.create[IO]
      implicit0(ctx: ExecutionContext) <- IO(s.dispatcher)
      implicit0(scheduler: Scheduler) <- IO(s.scheduler)
      implicit0(sessionScheduler: SessionScheduler) <- IOScheduler.create
      lotSessionStore <- LotSessionStore.fromResource("/sessions.json")
      _ <- lotSessionStore.scheduleStartAll
      lotSessionRoute = LotSessionHttp.route("session", lotSessionStore)
      _ <- runServer(lotSessionRoute)
      _ <- IO.never
    } yield ExitCode.Success
}

final case class HttpConfig(
  ip: String = "0.0.0.0",
  port: Int = 8080
)
