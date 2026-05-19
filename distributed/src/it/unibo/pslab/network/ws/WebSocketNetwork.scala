package it.unibo.pslab.network.ws

import it.unibo.pslab.network.{
  ScalaTropyMessage,
  BaseNetwork,
  Network,
  NetworkMonitor,
  NoSuchPeers,
  PeerId,
  PeerRef,
  WS,
}
import it.unibo.pslab.network.BaseNetwork.IncomingMessages
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.effect.{ Async, Concurrent, Ref }
import cats.effect.std.Queue
import cats.syntax.all.*
import cats.effect.syntax.all.*
import sttp.ws.WebSocketFrame
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import fs2.io.net.Network as Fs2Network

trait WebSocketNetwork[F[_], LP <: Peer] extends Network[F, LP, PeerRef], WS

object WebSocketNetwork:

  def make[F[_]: {Async, Concurrent, Fs2Network, NetworkMonitor}, LP <: Peer: PeerTag as localPeer](
      ipv4Address: String,
      port: Int,
      knownPeers: Set[PeerRef[?]],
  ): F[WebSocketNetwork[F, LP]] =
    for
      alivePeers <- Ref.of[F, Map[PeerId, Queue[F, WebSocketFrame]]](Map.empty)
      connectedPeers <- Ref.of[F, Set[PeerRef[?]]](knownPeers)
      incomingMsgs <- Ref.of[F, IncomingMessages[F, PeerRef[?]]](IncomingMessages.empty)
      impl = WebSocketNetworkImpl(ipv4Address, port, connectedPeers, alivePeers, incomingMsgs)
      _ <- EmberServerBuilder
        .default[F]
        .withHost(Host.fromString(ipv4Address).get) // TODO: handle invalid address
        .withPort(Port.fromInt(port).get) // TODO: handle invalid port
        .withHttpWebSocketApp(impl.setup(_).orNotFound)
        .build
        .useForever
        .start
    yield impl

  private class WebSocketNetworkImpl[
      F[_]: {Async, Concurrent, NetworkMonitor},
      LP <: Peer: PeerTag as localPeer,
  ](
      ipv4Address: String,
      port: Int,
      connectedPeers: Ref[F, Set[PeerRef[?]]],
      protected val alivePeers: Ref[F, Map[PeerId, Queue[F, WebSocketFrame]]],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerRef[?]]],
  ) extends WebSocketNetwork[F, LP],
        WebSocketAcceptor[F],
        WebSocketConnector[F],
        BaseNetwork[F, LP]:

    override val local: PeerRef[LP] = PeerId(localPeer, s"ws://${ipv4Address}:${port}/ws")

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeer]: F[NonEmptyList[PeerId]] =
      connectedPeers.get.flatMap: peers =>
        val filtered = peers.filter(_.tag <:< remotePeer)
        NonEmptyList.fromList(filtered.toList) match
          case Some(nel) => nel.pure
          case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeer))

    override def dispatch[To <: Peer: PeerTag](to: PeerRef[To], message: ScalaTropyMessage): F[Unit] =
      emit[To, ScalaTropyMessage](to, message)

    override def onMessage(payload: Array[Byte]): F[Unit] =
      for
        ScalaTropyMessage(address, resource, data) <- F.catchNonFatal(upickle.readBinary[ScalaTropyMessage](payload))
        d <- takePeerMsgOrDefer((address, resource))
        _ <- d.complete(data)
      yield ()
  end WebSocketNetworkImpl
end WebSocketNetwork
