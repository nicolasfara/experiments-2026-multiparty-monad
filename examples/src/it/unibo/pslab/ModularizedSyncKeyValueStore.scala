package it.unibo.pslab

import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import KeyValueStore.inMemory as inMemoryKeyValueStore
import UpickleCodable.given
import cats.{ Applicative, Monad }
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Sync
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

object ModularizedSyncKeyValueStoreChoreo:

  type Client <: { type Tie <: Single[Primary] }
  type Primary <: { type Tie <: Multiple[Backup] & Multiple[Client] }
  type Backup <: { type Tie <: Single[Primary] }

  enum Request derives ReadWriter:
    case Get(key: String)
    case Put(key: String, value: String)
    case Empty

  enum Response derives ReadWriter:
    case Value(value: Option[String])
    case Ack
    case Empty

  import Request.*

  type RequestsHandler[F[_]] =
    (lang: MultiParty[F]) ?=> lang.Anisotropic[Client, Request] on Primary => F[lang.Anisotropic[Client, Response]]

  type BackupHandler[F[_]] = (lang: MultiParty[F]) ?=> List[Request.Put] on Backup => F[Unit]

  def app[F[_]: {Sync, Console}](using lang: MultiParty[F]): F[Unit] =
    for
      storeHandle <- on[Primary](storeHandler[F])
      backupHandle <- on[Backup](backupHandler[F])
      _ <- kvs(storeHandle, backupHandle)
    yield ()

  def kvs[F[_]: {Sync, Console}](
      handler: RequestsHandler[F] on Primary,
      backupHandler: BackupHandler[F] on Backup,
  )(using MultiParty[F]): F[Unit] =
    for
      requestOnClient <- on[Client](waitForRequest)
      requestsOnPrimary <- coAnisotropicComm[Client, Primary](requestOnClient)
      responsesOnPrimary <- on[Primary]:
        take(handler) flatMap (_(requestsOnPrimary))
      putRequests <- on[Primary]:
        takeAll(requestsOnPrimary) map (_.values.collect { case r: Put => r }.toList)
      requestsOnBackups <- isotropicComm[Primary, Backup](putRequests)
      ack <- on[Backup]:
        take(backupHandler) flatMap (_(requestsOnBackups))
      _ <- coAnisotropicComm[Backup, Primary](ack)
      responseOnClient <- anisotropicComm[Primary, Client](responsesOnPrimary)
      _ <- on[Client]:
        take(responseOnClient) flatMap (response => F.println(s"> $response"))
      _ <- kvs(handler, backupHandler)
    yield ()

  def storeHandler[F[_]: Sync](using Label[Primary]): F[RequestsHandler[F]] =
    inMemoryKeyValueStore[F, String, String].map: store =>
      requestsOnPrimary =>
        for
          requests <- takeAll(requestsOnPrimary)
          responses <- store processAll requests
          message <- anisotropicMessage[Primary, Client](responses, Response.Empty)
        yield message

  def backupHandler[F[_]: Sync](using Label[Backup]): F[BackupHandler[F]] =
    inMemoryKeyValueStore[F, String, String].map: store =>
      requests =>
        for
          rqs <- take(requests)
          _ = rqs.foreach(request => store process request)
        yield ()

  def waitForRequest[F[_]: {Monad, Console}]: F[Request] =
    F.println("Enter a request ([get|put] <key> [value]):") >> F.readLine
      .map(_.parse)
      .flatMap:
        case Left(error) => F.errorln(s"Invalid request: $error") >> waitForRequest
        case Right(req)  => req.pure

  extension (s: String)
    def parse: Either[String, Request] =
      s.trim.split("\\s+", 3) match
        case Array("get", key)        => Right(Request.Get(key))
        case Array("put", key, value) => Right(Request.Put(key, value))
        case _                        => Left("Use: get <key> or put <key> <value>")

  extension [F[_]: Applicative](store: KeyValueStore[F, String, String])
    infix def processAll(using lang: MultiParty[F])(requests: Map[lang.Remote[Client], Request]) =
      requests.toList
        .traverse((client, request) => (store process request).tupleLeft(client))
        .map(_.toMap)

    infix def process(req: Request): F[Response] =
      req match
        case Request.Get(key)        => store.get(key).map(Response.Value(_))
        case Request.Put(key, value) => store.put(key, value).map(_ => Response.Ack)
        case Request.Empty           => Response.Empty.pure

import ModularizedSyncKeyValueStoreChoreo.*

object MPrimaryNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val net = MqttNetwork.localBroker[IO, Primary](Configuration(appId = "mkvs"))
    ScalaTropy(app[IO]).projectedOn[Primary](using net)

object MBackupNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val net = MqttNetwork.localBroker[IO, Backup](Configuration(appId = "mkvs"))
    ScalaTropy(app[IO]).projectedOn[Backup](using net)

object MClient1Node extends IOApp.Simple:
  override def run: IO[Unit] =
    val net = MqttNetwork.localBroker[IO, Client](Configuration(appId = "mkvs"))
    ScalaTropy(app[IO]).projectedOn[Client](using net)

object MClient2Node extends IOApp.Simple:
  override def run: IO[Unit] =
    val net = MqttNetwork.localBroker[IO, Client](Configuration(appId = "mkvs"))
    ScalaTropy(app[IO]).projectedOn[Client](using net)
