package it.unibo.pslab.network.ws

import it.unibo.pslab.network.PeerId

import cats.effect.{ Async, Ref }
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.implicits.{ catsSyntaxApplyOps, toFlatMapOps, toFunctorOps }
import cats.syntax.all.*
import fs2.{ Pipe, Stream }
import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.network.PeerRef
import it.unibo.pslab.peers.Peers.PeerTag
import upickle.default.Writer

trait WebSocketHandler[F[_]]:
  def onMessage(payload: Array[Byte]): F[Unit]

trait WebSocketAcceptor[F[_]: Async] extends WebSocketHandler[F]:
  import org.http4s.*
  import org.http4s.dsl.io.*
  import org.http4s.server.websocket.WebSocketBuilder2
  import org.http4s.websocket.WebSocketFrame

  def setup(wsb: WebSocketBuilder2[F]): HttpRoutes[F] = HttpRoutes.of[F]:
    case GET -> Root / "ws" =>
      val pipe: Pipe[F, WebSocketFrame, WebSocketFrame] = incoming =>
        incoming
          .evalMap:
            case WebSocketFrame.Binary(payload, true) => onMessage(payload.toArray)
            case WebSocketFrame.Text(payload, true)   => onMessage(payload.getBytes)
            case _                                    => F.unit
          .drain
      wsb.build(pipe)

trait WebSocketConnector[F[_]: Async] extends WebSocketHandler[F]:
  import sttp.capabilities.fs2.Fs2Streams
  import sttp.client4.*
  import sttp.client4.httpclient.fs2.HttpClientFs2Backend
  import sttp.client4.ws.stream.*
  import sttp.ws.WebSocketFrame
  import sttp.model.Uri

  protected val alivePeers: Ref[F, Map[PeerId, Queue[F, WebSocketFrame]]]

  def emit[To <: Peer: PeerTag, Value: Writer](to: PeerRef[To], data: Value): F[Unit] =
    for
      q <- alivePeers.get.flatMap(peers => peers.get(to).map(_.pure).getOrElse(bootstrap(to)))
      payload <- F.catchNonFatal(upickle.writeBinary(data))
      _ <- F.catchNonFatal(
        println(s"[WebSocketNetwork] sending message to ${to.id} with payload size ${payload.length}"),
      )
      _ <- q.offer(WebSocketFrame.binary(payload))
    yield ()

  private def bootstrap(to: PeerId): F[Queue[F, WebSocketFrame]] =
    for
      q <- Queue.unbounded[F, WebSocketFrame]
      _ <- F.catchNonFatal(println(s"[WebSocketNetwork] starting connection to ${to.id}"))
      _ <- openConnection(
        url = to.id,
        pipe = incoming =>
          val incomingHandler = incoming.evalMap:
            case WebSocketFrame.Text(payload, true, _)   => onMessage(payload.getBytes)
            case WebSocketFrame.Binary(payload, true, _) => onMessage(payload)
            case _                                       => F.unit
          Stream.repeatEval(q.take).concurrently(incomingHandler),
      ).guarantee(alivePeers.update(_ - to)).start
    yield q

  private def openConnection(url: String, pipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame]): F[Unit] =
    HttpClientFs2Backend // TODO: parametrize
      .resource[F]()
      .use: backend =>
        F.catchNonFatal(println(s"[WebSocketNetwork] opening ws client to $url")) *>
          basicRequest
            .get(Uri.unsafeParse(url)) // TODO: handle invalid URL
            .response(asWebSocketStream(Fs2Streams[F])(pipe))
            .send(backend)
            .void
end WebSocketConnector
